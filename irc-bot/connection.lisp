(in-package :irc-bot)

(defclass bot ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (nickname :initarg :nickname
             :initform nil
             :accessor nickname)
   (password :initarg :password
             :initform nil
             :accessor password)
   (server :initarg :server
           :initform nil
           :accessor server)
   (channels :initarg :channels
             :initform nil
             :accessor channels)
   (connection :initarg :connection
               :initform nil
               :accessor connection)
   (thread :initarg :thread
           :initform nil
           :accessor thread)
   (nick-retry-count :initarg :nick-retry-count
                     :initform nil
                     :accessor nick-retry-count)
   (ping-timeout :initarg :ping-timeout
                 :initform *ping-timeout*
                 :accessor ping-timeout)))

(defvar *ping-timeout* 45)

(defvar *debug* nil)
(defvar *bots* nil)
(defvar *bot*)

(defmethod stop (bot)
  (usocket:socket-close (irc::socket (connection bot)))
  (bt:destroy-thread (thread bot)))

(defun read-messages (connection)
  (if (listen (network-stream connection))
      (handler-bind
          ;; install sensible recovery: nobody can wrap the
          ;; handler...
          ((no-such-reply
             #'(lambda (c)
                 (declare (ignore c))
                 (invoke-restart 'continue))))
        (read-message connection))
      ;; select() returns with no
      ;; available data if the stream
      ;; has been closed on the other
      ;; end (EPIPE)
      (throw 'connection :restart)))

(defun wait-on-fd (fd)
  (handler-case (iomux:wait-until-fd-ready
                 fd :input *ping-timeout*)
    (iomux:poll-error (c)
      (princ c)
      (terpri)
      (throw 'connection :restart))))

(defun start-bot-loop (connection)
  (let ((last-communication (get-universal-time))
        (fd (sb-sys:fd-stream-fd
             (network-stream connection)))
        ping-sent)
    (loop for usable = (wait-on-fd fd)
          for time = (get-universal-time)
          do (cond (usable
                    (setf last-communication time
                          ping-sent nil)
                    (read-messages connection))
                   ((< (- time last-communication) *ping-timeout*))
                   (ping-sent
                    (write-line "restarting minion")
                    (finish-output)
                    (throw 'connection :restart))
                   (t
                    (setf ping-sent t)
                    (when *debug*
                      (format t "~a ping" (name *bot*)))
                    (finish-output)
                    (ping connection (server-name connection)))))))

(defun try-connecting (bot)
  (with-slots (connection nickname server
               nick-retry-count)
      bot
    (setf connection
          (loop thereis
                (handler-case (connect :nickname nickname :server server)
                  ((or usocket:socket-condition
                    usocket:ns-condition) (c)
                    (princ c)
                    (terpri)
                    nil))
                do
                (sleep 5)))))

(defmethod setup-connection (bot)
  (with-slots (connection nick-retry-count
               server) bot
   (loop always
         (eql :restart
              (catch 'connection
                (setf nick-retry-count 0)
                (try-connecting bot)
                (print connection)
                (add-hook connection 'irc-privmsg-message 'msg-hook)
                (add-hook connection 'irc-notice-message 'notice-hook)
                (add-hook connection 'irc-err_nicknameinuse-message 'in-use-hook)
                (add-hook connection 'irc-pong-message 'pong-hook)
                (start-bot-loop connection)))
         do
         (usocket:socket-close (irc::socket connection)))))

(defun identify (bot)
  (with-slots (connection nickname channels
               password) bot
    (nick connection nickname)
    (when password
      (privmsg connection "NickServ"
               (format nil "IDENTIFY ~A" password)))))

(defun notice-hook (message)
  (cond ((not (equal (source message) "NickServ"))
         nil)
        ((search "nickname is registered"
                 (message-body message))
         (identify *bot*))
        ((search "You are now identified for"
                 (message-body message))
         (mapcar (lambda (channel) (join (connection *bot*) channel))
                 (channels *bot*)))))

(defun ghost (bot)
  (with-slots (connection nickname password) bot
    (privmsg connection "NickServ"
             (format nil "GHOST ~a ~a" nickname password))
    (nick connection nickname)))

(defun in-use-hook (message)
  (declare (ignore message))
  (with-slots (connection nickname nick-retry-count) *bot*
    (nick connection
          (format nil "~a~a" nickname (incf nick-retry-count)))
    (ghost *bot*)))

(defun pong-hook (message)
  (declare (ignore message))
  (when *debug*
   (write-line "pong"))
  t)

(defmethod start (bot &key initialize)
  (when initialize
    (initialize-bot-from-config bot))
  (pushnew bot *bots*)
  (setf (thread bot)
        (bt:make-thread (lambda ()
                          (let ((*bot* bot))
                            (setup-connection bot)))
                        :name (name bot))))

(defmethod start ((bot symbol) &key)
  (start (make-instance bot) :initialize t))

(defun split-message (text)
  (ppcre:register-groups-bind (for message) ("^(?:(\\w+)[,:]\\s+)?(.*)" text)
    (values for message)))

(defun message-body (message)
  (car (last (arguments message))))

(defun message-destination (message)
  (car (arguments message)))

(defun msg-hook (message)
  (let ((body (message-body message))
        (destination (message-destination message))
        (sender (source message)))
    (multiple-value-bind (for text) (split-message body)
      (process-all-messages *bot* destination sender for text body))
  t))

(defgeneric send-message (bot destination text))

(defmethod send-message (bot destination text)
  (when text
    (privmsg (connection bot) destination text)))

(defgeneric process-all-messages (bot channel sender for-nick text full-text))
(defgeneric process-message-for-bot (bot channel sender text full-text))
(defgeneric process-private-message (bot sender text full-text))
(defgeneric process-message (bot channel sender for-nick text full-text))

(defmethod process-all-messages (bot channel sender for-nick text full-text)
  (cond ((equalp (nickname bot) channel)
         (process-private-message bot sender text full-text))
        ((equalp (nickname bot) for-nick)
         (process-message-for-bot bot channel sender text full-text))
        (t
         (process-message bot channel for-nick sender text full-text))))

(defmethod process-message-for-bot (bot channel sender text full-text))
(defmethod process-private-message (bot sender text full-text))
(defmethod process-message (bot channel sender for-nick text full-text))

(defmethod initialize-bot-from-config (bot)
  (let ((file (make-pathname :name (format nil ".~a" (name bot))
                             :type "rc"
                             :defaults (user-homedir-pathname))))
    (with-open-file (stream file :if-does-not-exist nil)
      (when stream
        (destructuring-bind (&key nickname channels
                                  password
                                  server) (read stream nil)
          (setf (nickname bot) (or nickname (nickname bot))
                (channels bot) (or channels (channels bot))
                (password bot) (or password (password bot))
                (server bot) (or server (server bot))))
        bot))))
