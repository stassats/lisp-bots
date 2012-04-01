(in-package :lisppaste)

(defvar *paste-ips*
  (make-hash-table :test #'equal))

(defun paste-allowed-from-ip-p (ip is-pasting)
  (when ip
    (if (find ip *banned-ips* :test #'equal)
	nil
	(if is-pasting
	    (let ((time (get-universal-time)))
	      (push time (gethash ip *paste-ips*))
	      (let ((new-times
		     (remove-if (lambda (n)
				  (>= (- time n) 15))
				(gethash ip *paste-ips*))))
		(setf (gethash ip *paste-ips*) new-times)
		(if (>= (length new-times)
			5)
		    (progn
		      (ban-ip ip)
		      nil)
		    t)))
	    t))))

(defun write-banned-ips ()
  (with-open-file (f *banned-ips-file* :direction :output :if-exists :supersede)
    (prin1 *banned-ips* f)))

(defun ban-ip (ip)
  (log-event (format nil "Banning ip ~A" ip)
	     :log-file *ban-log-file*)
  (pushnew ip *banned-ips* :test #'equal)
  (write-banned-ips))

(defun unban-ip (&optional (ip (car *banned-ips*)))
  (log-event (format nil "Unbanning ip ~A" ip)
	     :log-file *ban-log-file*)
  (setf *banned-ips* (remove ip *banned-ips* :test #'equal))
  (write-banned-ips))