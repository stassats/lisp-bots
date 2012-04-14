;;;; specbot.lisp - an example IRC bot for cl-irc

;;; specbot is an example IRC bot for cl-irc. It runs on
;;; irc.freenode.net in the channels #lisp, #scheme and #clhs
;;; (preferred for testing). It responds to queries of its various
;;; databases, which right now include "clhs" and "r5rs".

;;; You will need to load and populate the tables for both the clhs
;;; and r5rs lookup packages; currently these are available in
;;; lisppaste CVS.

;;; To use it, load the cl-irc system, load specbot.lisp, and
;;; invoke (specbot:start-specbot "desirednickname" "desiredserver"
;;; "#channel1" "#channel2" "#channel3" ...)


(in-package :specbot)

(defvar *base-path*
  (asdf:system-relative-pathname :specbot "specbot/"))

(defvar *connection*)
(defvar *nickname* "")

(defun shut-up ()
  (setf (irc:client-stream *connection*) (make-broadcast-stream)))

(defun un-shut-up ()
  (setf (irc:client-stream *connection*) *trace-output*))

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defun clhs-lookup (str)
  (multiple-value-bind (url term)
      (spec-lookup:lookup "clhs" str)
    (when url
      (let ((full-term (unless (equal str (spec-lookup:key term))
                         (spec-lookup:key term))))
        (format nil "~@[~a: ~]~@[~a: ~]~a"
                full-term (spec-lookup:title term) url)))))

(defun r5rs-lookup (str)
  (and (find-package :r5rs-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :r5rs-lookup)
                str)))

(defun cocoa-lookup (str)
  (and (find-package :cocoa-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :cocoa-lookup)
                str)))

(defun elisp-lookup (str)
  (and (find-package :elisp-lookup)
       (funcall (intern "SYMBOL-LOOKUP" :elisp-lookup)
                str)))

(defun clim-lookup (str)
  (and (find-package :clim-lookup)
       (funcall (intern "TERM-LOOKUP" :clim-lookup)
                str)))

(defun launchpad-lookup (str)
  ;; XXX: May want to validate STR a bit (numeric only? strip starting
  ;; #?)  Add to the mystique: It's a redirect, why not chase it and
  ;; return the redirected URL?  Make it more useful: If we do pull
  ;; the redirect we can also pull the bug title, affected projects,
  ;; etc.
  (when (and (plusp (length str))
	     (char-equal (aref str 0) #\#))
    (setf str (subseq str 1)))
  (format nil "https://bugs.launchpad.net/bugs/~A" str))

(defvar *spec-providers*
  '((clhs-lookup "clhs" "The Common Lisp HyperSpec")
    (r5rs-lookup "r5rs" "The Revised 5th Ed. Report on the Algorithmic Language Scheme")
    (cocoa-lookup "cocoa" "Classes in the Cocoa Foundation and Application kits")
    (elisp-lookup "elisp" "GNU Emacs Lisp Reference Manual")
    (clim-lookup "clim" "Common Lisp Interface Manager II Specification")
    (launchpad-lookup "lp" "Bugs tracked on Launchpad")))

(defvar *spaces-allowed*
  '(clim-lookup (simple-alist-lookup cltl2-sections)))

(defvar *alists* nil)

(defun add-simple-alist-lookup (file designator prefix description)
  (unless (assoc designator *alists*)
    (let ((alist (with-open-file (s file :direction :input) (read s))))
      (push (cons designator alist) *alists*)
      (setf *spec-providers*
            (nconc *spec-providers*
                   (list `((simple-alist-lookup ,designator) ,prefix ,description)))))))

(defun simple-alist-lookup (designator string)
  (let ((alist (cdr (assoc designator *alists*))))
    (cdr (assoc string alist :test #'equalp))))

(defun valid-message (string prefix &key space-allowed)
  (if (eql (search prefix string :test #'char-equal) 0)
      (and (or space-allowed
               (not (find #\space string :start (length prefix))))
           (length prefix))
      nil))

(defun strip-address (string &key (address *nickname*) (final nil))
  (loop for i in (list (format nil "~A " address)
                       (format nil "~A: " address)
                       (format nil "~A:" address)
                       (format nil "~A, " address))
        do (aif (valid-message string i :space-allowed t)
                (return-from strip-address (subseq string it))))
  (and (not final) string))

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nickname*)
                         (source message)
                         (first (arguments message))))
        (to-lookup (strip-address (trailing-argument message))))
    (if (and (or
              (string-equal (first (arguments message)) *nickname*)
              (not (string= to-lookup (trailing-argument message))))
             (member to-lookup '("help" "help?") :test #'string-equal))
        (progn
          (privmsg *connection* destination
                   (format nil "To use the ~A bot, say something like \"database term\", where database is one of (~{~S~^, ~}) and term is the desired lookup. The available databases are:"
                           *nickname*
                           (mapcar #'second *spec-providers*)))
          (loop for i from 1 for j in *spec-providers*
                with elts = nil
                do (push j elts)
                if (zerop (mod i 4))
                do (progn
                     (privmsg *connection* destination
                              (format nil "~{~{~*~S, ~A~}~^; ~}"
                                      (nreverse elts)))
                     (setf elts nil)))
	  )
        (loop for type in *spec-providers*
              for actual-fun = (if (typep (first type) 'symbol)
                                   (first type)
                                   (lambda (lookup) (destructuring-bind (fun first-arg) (first type)
                                                      (funcall fun first-arg lookup))))
              do
              (aif (strip-address to-lookup :address (second type) :final t)
                   (let ((looked-up (funcall actual-fun it)))
                     (if (and (<= 0 (count #\space it)
				  (if (member (first type) *spaces-allowed* :test #'equal) 1 0) 1)
                              (not looked-up))
                         (setf looked-up (format nil "Sorry, I couldn't find anything for ~A."  it)))
                     (and looked-up
                          (privmsg *connection* destination looked-up))))))))

(defparameter *754-file*
  (merge-pathnames "754.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *ppc-file*
  (merge-pathnames "ppc-assem.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *sus-file*
  (merge-pathnames "sus.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *man-file*
  (merge-pathnames "man.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *cltl2-file*
  (merge-pathnames "cltl2.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *cltl2-sections-file*
  (merge-pathnames "cltl2-sections.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defvar *last-communication-time* nil)
(defvar *ping-sent* nil)
(defvar *fd-handler* nil)
(defvar *timer* nil)

(defun start-handler (connection)
  (flet ((select-handler (fd)
           (declare (ignore fd))
           (if (listen (network-stream connection))
               (handler-bind
                   ;; install sensible recovery: nobody can wrap the
                   ;; handler...
                   ((no-such-reply
                      #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'continue))))
                 (setf *last-communication-time* (get-universal-time))
                 (read-message connection))
               ;; select() returns with no
               ;; available data if the stream
               ;; has been closed on the other
               ;; end (EPIPE)
               (sb-sys:invalidate-descriptor
                (sb-sys:fd-stream-fd
                 (network-stream connection))))))
    (sb-sys:add-fd-handler (sb-sys:fd-stream-fd
                            (network-stream connection))
                           :input #'select-handler)))

(defun ping-hook (message)
  (setf *ping-sent* nil)
  (apply #'pong (connection message) (arguments message)))

(defvar *ping-timeout* 30)

(defun stop-specbot ()
  (sb-sys:remove-fd-handler *fd-handler*)
  (sb-ext:unschedule-timer *timer*)
  (setf *fd-handler* nil
        *timer* nil
        *ping-sent* nil
        *last-communication-time* nil)
  (usocket:socket-close (irc::socket *connection*)))

(defun restart-specbot ()
  (stop-specbot)
  (apply #'start-specbot
         (nickname (user *connection*))
         (server-name *connection*)
         (alexandria:hash-table-keys (channels *connection*))))

(defun connection-poller ()
  (cond ((< (- (get-universal-time) *last-communication-time*) 30))
        (*ping-sent*
         (restart-specbot))
        (t
         (print "sending ping")
         (ping *connection*  (server-name *connection*)))))

(defun start-bot-loop (connection)
  (setf *timer* (sb-ext:make-timer #'connection-poller :name "specbot poller")
        *last-communication-time* (get-universal-time)
        *ping-sent* nil
        *fd-handler* (start-handler connection))
  (sb-ext:schedule-timer *timer* 40 :repeat-interval 30))

(defun start-specbot (nick server &rest channels)
  (spec-lookup:read-specifications)
  (add-simple-alist-lookup *754-file* 'ieee754 "ieee754" "Section numbers of IEEE 754")
  (add-simple-alist-lookup *ppc-file* 'ppc "ppc" "PowerPC assembly mnemonics")
  (add-simple-alist-lookup *sus-file* 'sus "posix" "Single UNIX Specification")
  (add-simple-alist-lookup *man-file* 'man "man" "Mac OS X Man Pages")
  (add-simple-alist-lookup *cltl2-file* 'cltl2 "cltl2" "Common Lisp, The Language (2nd Edition)")
  (add-simple-alist-lookup *cltl2-sections-file* 'cltl2-sections "cltl2-section" "Common Lisp, The Language (2nd Edition) Sections")
  (setf *nickname* nick)
  (setf *connection* (connect :nickname *nickname* :server server))
  (mapcar #'(lambda (channel) (join *connection* channel)) channels)
  (add-hook *connection* 'irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc-ping-message 'ping-hook)
  (start-bot-loop *connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *connection* 'irc::irc-privmsg-message)
  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook))
