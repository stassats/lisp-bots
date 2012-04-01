(in-package :cl-user)

(asdf:load-system :lisppaste)
(asdf:load-system :specbot)

(clim-lookup:populate-table)

(defparameter *minion-channels* '("#lisp-repl";; "#clhs" "#ifdef" "#lisp" "#scheme" "#tech.coop" "#abcl" "#lisppaste-status" "#sbcl" "#lispgames" "#iolib"
                                  ))
(defparameter *specbot-channels* '("#lisp-repl";; "#clhs" "#ifdef" "#lisp" "#emacs" "#scheme" "#macdev" "#concatenative" "#abcl" "#lisppaste-status" "#sbcl" "#lispgames" "#iolib"
                                   ))

(let ((*default-pathname-defaults* (merge-pathnames "cl-irc/example/"
                                                    *load-dir*)))
  (setf cliki::*cliki-nickserv-password* "l1sp")
  (sleep 5)
  ;; (apply #'cliki::start-cliki-bot "minion2" "chat.freenode.net" *minion-channels*)
  ;; (sleep 5)
  (apply #'specbot::start-specbot "specbot2" "chat.freenode.net" *specbot-channels*)
  (sleep 5))

;(load (compile-file "panacea"))

;; (sb-sys:enable-interrupt sb-unix:sigusr1
;; 			 (lambda (signal info context)
;; 			   (declare (ignore signal info context))
;; 			   (panacea)
;; 			   (invoke-restart (find-restart 'abort))))

(load (compile-file "usr2-backtrace"))

;;; SBCL, when compiled with :SB-LDB, tends to end up in an
;;; interactive low-level debugger when something nasty happens like
;;; heap memory is corrupted, total memory exhaustion, signal handling
;;; issues, etc.  We'd rather the process just die when that happens
;;; so that other software can restart it.  The magic incantation is:
#+(and sbcl (not swank))
(sb-ext:disable-debugger)

(defun shut-up ()
  (lisppaste::shut-up)
  (cliki::shut-up)
  (specbot::shut-up))

;(shut-up)
