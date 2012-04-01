(cl:in-package :cl-user)

(defun today-date-string ()
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (declare (ignore sec min hour))
    (format nil "~4,'0D~2,'0D~2,'0D" year month date)))

(defun backtrace-counter ()
  (let ((counter 0)
	(today (today-date-string)))
    (loop for pathname in (directory
			   (merge-pathnames
			    (format nil "backtrace-~A-*" today)
			    (user-homedir-pathname)))
	 do (let ((num (parse-integer
			(first (last
				(split-sequence:split-sequence #\-
							       (pathname-name pathname))))
			:junk-allowed t)))
	      (when (> num counter)
		(setf counter num))))
    (format nil "backtrace-~A-~A" today (1+ counter))))

(sb-sys:enable-interrupt sb-unix:sigusr2
  (lambda (signal info context)
    (declare (ignore signal info context))
    (with-open-file
	(file-stream
	 (merge-pathnames
	  (make-pathname :name
			 (backtrace-counter))
	  (user-homedir-pathname))
	 :direction :output
	 :if-exists :error
	 :if-does-not-exist :create)
      (let ((b-stream (make-broadcast-stream file-stream *debug-io*)))
	(terpri b-stream)
	(backtrace most-positive-fixnum b-stream)))))

