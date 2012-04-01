;;;; $Id: irc-notification.lisp,v 1.14 2010-05-29 14:19:47 lisppaste Exp $
;;;; $Source: /project/lisppaste/cvsroot/lisppaste2/irc-notification.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :lisppaste)

(defvar *connections* nil)
(defvar *nicknames* nil)
(defparameter *channel-limit* 19)
(defparameter *status-channel* "#lisppaste-status")
;; FIXME: should be in variable.lisp

(defun find-free-nick ()
  (cdr (find-if (lambda (pair)
		  (< (length (car pair)) *channel-limit*))
		(reverse *nicknames*))))

(defun channel-nick (channel)
  (cdr (assoc channel *nicknames*
		    :test #'(lambda (e s)
			      (member e s :test #'string-equal)))))

(defun nick-connection (nick)
  (cdr (assoc nick *connections* :test #'string-equal)))

(defun find-connection (channel)
  (nick-connection (channel-nick channel)))

(defun irc-say-help (channel)
  (when (and (find-connection channel)
             (find channel *channels* :test #'string-equal))
    (irc:privmsg (find-connection channel)
                 channel
                 (format nil "To use the lisppaste bot, visit ~A/~A and enter your paste."
                         (full-url *new-paste-url*)
                         (urlstring-escape (subseq channel 1))))
    t))

(defun excluding-trailing-digits (nick)
  (coerce
   (loop for i from (1- (length nick)) downto 0
      if (not (digit-char-p (elt nick i)))
      return (subseq nick 0 (1+ i)))
   'string))

(defun help-request-p (nick help text)
  (and (> (length text)
          (length nick))
       (search nick text :start2 0 :end2 (length nick) :test #'char-equal)
       (let ((url-position (search help text :start2 (length nick)
                                   :test #'char-equal)))
         (and
          url-position
          (notany #'alpha-char-p (subseq text (length nick) (1- url-position)))
          (notany #'alpha-char-p (subseq text (+ url-position (length help))))))))

(defun make-irc-msg-hook (connection nick)
  (lambda (message)
    (let* ((arguments (irc:arguments message))
           (text (car (last arguments)))
           (message-nick (first arguments)))
      (cond ((string-equal message-nick nick)
             (irc:privmsg connection
                          (irc:source message)
                          (format nil "To use the lisppaste bot, visit ~A and enter your paste. Be sure to select the right channel!"
                                  (full-url *new-paste-url*)))
	     ;; KLUDGE: keep from flooding off
	     (sleep 0.05))
            ((some #'(lambda (e)
                       (help-request-p (excluding-trailing-digits nick) e text))
                   '("url" "help" "hello"))
             (irc-say-help message-nick))))))

(defun add-irc-hook (connection nick)
  (irc:remove-hooks connection 'irc:irc-privmsg-message)
  (irc:add-hook connection 'irc:irc-privmsg-message (make-irc-msg-hook connection nick)))

(defun start-irc-notification (&key (channels (list *default-channel*))
                               (nickname *default-nickname*)
                               (server *default-irc-server*)
                               (port *default-irc-server-port*))
  (let ((connection (irc:connect :nickname nickname
                                 :realname (full-url *new-paste-url*)
                                 :server server
                                 :port port)))
    (push (cons nickname connection) *connections*)
    (setf channels
	  (mapcar (lambda (channel)
		    (cond ((consp channel)
			   (destructuring-bind (channel coloring-type) channel
			     (setf (gethash channel *coloring-type-defaults*)
				   coloring-type)
			     channel))
			  (t channel)))
		  channels))
    (setf *channels* (append *channels* channels))
    (push (cons (copy-list channels) nickname) *nicknames*)
    (mapcar #'(lambda (channel) (irc:join connection channel)) channels)
    (when *status-channel*
      (irc:join connection *status-channel*))
    (add-irc-hook connection nickname)
    (irc:start-background-message-handler connection)
    (sleep 5)))

(defun stop-irc-notification (nickname)
  (ignore-errors (irc:quit (nick-connection nickname)))
  (loop for i in (car (rassoc nickname *nicknames* :test #'string-equal))
     do (setf *channels* (remove i *channels* :test #'string-equal)))
  (setf *nicknames* (remove nickname *nicknames* :key #'cdr :test #'string-equal))
  (setf *connections* (remove nickname *connections* :key #'car :test #'string-equal)))

(defun join-new-irc-channel (nickname channel &optional coloring-type)
  (push channel (car (rassoc nickname *nicknames* :test #'string-equal)))
  (irc:join (find-connection channel) channel)
  (when coloring-type
    (setf (gethash channel *coloring-type-defaults*)
	  coloring-type))
  (setf *channels* (nconc *channels* (list channel))))

(defun leave-irc-channel (nickname channel)
  (setf *channels* (remove channel *channels* :test #'string-equal))
  (irc:part (find-connection channel) channel)
  (setf (car (rassoc nickname *nicknames* :test #'string-equal))
	(remove channel (car (rassoc nickname *nicknames* :test #'string-equal))
		:test #'string-equal)))

(defun make-quit-msg (nickname)
  (format nil "Want ~A in your channel? Email ~{~A~^ AT ~}." nickname (split-sequence:split-sequence #\@ *owner-email*)))

(defun quit-all-connections ()
  (mapc #'(lambda (e)
            (ignore-errors
	      (irc:quit (cdr e)
			(make-quit-msg (car e)))))
	*connections*))

(defun hup-all-connections ()
  (mapc #'hup-irc-connection (mapcar #'car *connections*)))

(defun hup-irc-connection (nickname &optional (server *default-irc-server*))
  (ignore-errors (irc:quit (nick-connection nickname) (make-quit-msg nickname)))
  (sleep 1)
  (setf
   (cdr (assoc nickname *connections* :test #'string-equal))
   (irc:connect :nickname nickname
		:realname (full-url *new-paste-url*)
		:server server
		:port *default-irc-server-port*))
  (mapcar #'(lambda (channel) (irc:join (nick-connection nickname) channel))
	  (car (rassoc nickname *nicknames* :test #'string-equal)))
  (when *status-channel*
    (irc:join (nick-connection nickname) *status-channel*))
  (add-irc-hook (nick-connection nickname) nickname)
  (irc:start-background-message-handler (nick-connection nickname)))

(defun %shut-up (connection)
  (setf (irc:client-stream connection)
	(make-broadcast-stream)))

(defun shut-up ()
  (mapc #'%shut-up (mapcar #'cdr *connections*)))

(defun %un-shut-up (connection)
  (setf (irc:client-stream connection) *trace-output*))

(defun un-shut-up ()
  (mapc #'%un-shut-up (mapcar #'cdr *connections*)))

(defun irc-notify (channel text)
  (let ((connection (find-connection channel)))
    (when connection
      (irc:privmsg connection channel
		   (remove-if (lambda (char)
				(or (eql char #\newline)
				    (eql char #\return)))
			      text)))))

(defun notify-all-channels (text)
  (loop for channel in *channels*
       do (irc-notify channel text)
       do (sleep 5)))
