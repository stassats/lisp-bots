;;;; $Id: lisppaste.lisp,v 1.31 2010-05-29 14:19:47 lisppaste Exp $
;;;; $Source: /project/lisppaste/cvsroot/lisppaste2/lisppaste.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :lisppaste)

(defun start-lisppaste ()
  "Start accepting web requests."
  (if *no-channel-pastes*
      (pushnew "None" *channels* :test #'string-equal))
  (read-xml-pastes)
  (format t "Populating lookup table...~%")
  (spec-lookup:read-specifications)
  (format t "Done!~%")
  (setf *boot-time* (get-universal-time))
  (start *acceptor*))

(defun logging-date ()
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D:~2,'0D "
	     month date year hour minute second)))

(defun truncate-string-to (string length)
  (subseq string 0 (min (length string) length)))

(defun log-event (text &key (log-file *event-log-file*))
  (with-open-file (s log-file :direction :output :if-exists :append
                     :if-does-not-exist :create)
    (write-string (logging-date) *trace-output*)
    (write-string (truncate-string-to text 50) *trace-output*)
    (fresh-line *trace-output*)
    (write-string (logging-date) s)
    (write-string text s)
    (finish-output s)))
