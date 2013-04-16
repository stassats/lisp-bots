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

(defclass specbot (irc-bot:bot)
  ()
  (:default-initargs :name "specbot"))

(defvar *base-path*
  (asdf:system-relative-pathname :specbot "specbot/"))


(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defun clhs-lookup (str)
  (multiple-value-bind (url term)
      (spec-lookup:lookup "clhs" str)
    (cond (url
           (if (consp term)
               url
               (let ((full-term (unless (equal str (spec-lookup:key term))
                                  (spec-lookup:key term))))
                 (format nil "~@[~a: ~]~@[~a: ~]~a"
                         full-term (spec-lookup:title term) url))))
          (t
           ;; skip the term if it's not valid
           (values nil (not term))))))

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
    ;; (clim-lookup "clim" "Common Lisp Interface Manager II Specification")
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

(defun help-message (bot destination)
  (irc-bot:send-message bot destination
                        (format nil "To use the ~A bot, say something~
 like \"database term\",~
where database is one of (~{~S~^, ~}) ~
and term is the desired lookup. "      ;The available databases are:
                                (nickname bot)
                                (mapcar #'second *spec-providers*)))
  ;; (loop for i from 1 for j in *spec-providers*
  ;;       with elts = nil
  ;;       do (push j elts)
  ;;       if (zerop (mod i 4))
  ;;       do
  ;;       (irc-bot:send-message bot destination
  ;;                             (format nil "~{~{~*~S, ~A~}~^; ~}"
  ;;                                     (nreverse elts)))
  ;;       (setf elts nil))
  )

(defun lookup-term (message)
  (loop for (handler name description) in *spec-providers*
        for handler-function = (if (consp handler)
                                   (lambda (term)
                                     (funcall (car handler)
                                              (cadr handler)
                                              term))
                                   handler)
        for term = (nth-value 1 (alexandria:starts-with-subseq (format nil "~a " name) message
                                                               :return-suffix t))
        when term
        return
        (multiple-value-bind (result skip)
            (funcall handler-function (string-trim " " term))
          (cond (skip
                 (values nil t))
                (result
                 result)
                (t
                 (format nil "Sorry, I couldn't find anything for ~A."
                         term))))))

(defun process-query (bot term destination)
  (multiple-value-bind (result skip)
      (lookup-term term)
    (unless skip
      (irc-bot:send-message bot destination result))))

(defmethod irc-bot:process-message ((bot specbot) channel sender for-nick text full-text)
  (process-query bot full-text channel))

(defmethod irc-bot:process-private-message ((bot specbot) sender text full-text)
  (if (member full-text '("help" "help?") :test #'equalp)
      (help-message bot sender)
      (process-query bot text sender)))

(defmethod irc-bot:process-message-for-bot ((bot specbot) channel sender text full-text)
  (when (member text '("help" "help?") :test #'equalp)
    (help-message bot channel)))

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

(defmethod irc-bot:start :before ((bot specbot) &key)
  (spec-lookup:read-specifications)
  (add-simple-alist-lookup *754-file* 'ieee754 "ieee754" "Section numbers of IEEE 754")
  (add-simple-alist-lookup *ppc-file* 'ppc "ppc" "PowerPC assembly mnemonics")
  (add-simple-alist-lookup *sus-file* 'sus "posix" "Single UNIX Specification")
  (add-simple-alist-lookup *man-file* 'man "man" "Mac OS X Man Pages")
  (add-simple-alist-lookup *cltl2-file* 'cltl2 "cltl2" "Common Lisp, The Language (2nd Edition)")
  (add-simple-alist-lookup *cltl2-sections-file* 'cltl2-sections "cltl2-section" "Common Lisp, The Language (2nd Edition) Sections"))
