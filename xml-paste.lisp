(in-package :lisppaste)

(defun ip-to-string (ip)
  (when ip
    (format nil "~{~A~^.~}" (coerce ip 'list))))

(defun paste-xml-list (paste &optional contents)
;  (format t "collecting paste number ~A~%" (paste-number paste))
  (list* (paste-number paste)
	 (s-xml-rpc:xml-rpc-time (paste-universal-time paste))
	 (paste-user paste)
	 (paste-channel paste)
	 (paste-title paste)
	 (length (paste-annotations paste))
	 (if contents
	     (list (remove #\return (paste-contents paste))))))

(defun xmlrpc-method-newpaste (args)
  (flet ((fail (reason)
	   (return-from xmlrpc-method-newpaste reason)))
    (format *trace-output* "Looking for ~S~%" (ip-to-string s-xml-rpc:*xml-peer-address*))
    (unless (paste-allowed-from-ip-p (ip-to-string s-xml-rpc:*xml-peer-address*))
      (fail "Naughty boy!"))
    (destructuring-bind
	  (paste-channel paste-user paste-title paste-contents
			 &optional annotate-or-colorize-as) args
      ;; Why not (every (lambda (s) (and (stringp s) (> (length s) 0))) ...)?
      (unless (every #'stringp (list paste-channel paste-user
				     paste-title paste-contents))
	(fail "Error: all arguments must be strings."))
      (unless (every (lambda (s) (> (length s) 0))
		     (list paste-channel paste-user
			   paste-title paste-contents))
	(fail "Error: all arguments must be non-empty strings."))
      (unless (<= (length paste-contents)
		  *paste-maximum-size*)
	(fail "Error: paste too large."))
      
      (let* ((annotate (if (numberp annotate-or-colorize-as) annotate-or-colorize-as))
	     (colorize-as (if (stringp annotate-or-colorize-as) annotate-or-colorize-as ""))
	     (annotate-this (if annotate (find-paste annotate)))
	     (paste-contents (remove #\return paste-contents)))
	(when (and annotate (not annotate-this))
	  (fail "Error: bad annotation number."))
	(unless (if annotate-this
		    (string-equal paste-channel (paste-channel annotate-this))
		    (member paste-channel *channels* :test #'string-equal))
	  (fail (format nil "Error: invalid channel ~S." paste-channel)))
	
	(when (some (lambda (regexp)
		 (or (cl-ppcre:scan regexp paste-contents)
		     (cl-ppcre:scan regexp paste-title)
		     (cl-ppcre:scan regexp paste-user))) *banned-content-regexps*)
	  (format *trace-output* "Banned content from ~S~%" (ip-to-string s-xml-rpc:*xml-peer-address*))
	  (when s-xml-rpc:*xml-peer-address*
	    (ban-ip (ip-to-string s-xml-rpc:*xml-peer-address*)))
	  (fail "Naughty boy!"))
	
	(let ((paste (make-new-paste annotate-this
				     :user paste-user
				     :title paste-title
				     :contents paste-contents
				     :channel paste-channel
				     :colorization-mode colorize-as)))
	  (log-new-paste (ip-to-string s-xml-rpc:*xml-peer-address*)
			 (paste-number paste) annotate paste-title)
	  (format nil "Your paste has been announced to ~A and is available at ~A ."
		  paste-channel (paste-display-url paste)))))))

(defun xmlrpc-method-pasteheaders (args)
  (destructuring-bind
	(length &optional supplied-start) args
    (mapcar #'paste-xml-list
	    (list-pastes :starting-from supplied-start
			 :limit length))))

(defun xmlrpc-method-pasteheadersbychannel (args)
  (destructuring-bind
	(channel length &optional supplied-start) args
    (mapcar #'paste-xml-list
	    (list-pastes :in-channel channel
			 :starting-from supplied-start
			 :limit length))))

(defun xmlrpc-method-pasteannotationheaders (args)
  (nreverse
   (mapcar #'paste-xml-list
	   (paste-annotations (find-paste (car args))))))

(defun xmlrpc-method-pastedetails (args)
  (destructuring-bind
	(paste-number &optional annotation) args
    (let ((paste (find-paste paste-number)))
      (if (not annotation)
	  (paste-xml-list paste t)
	  (paste-xml-list (find annotation
				(paste-annotations paste)
				:key #'paste-number :test #'eql)
			  t)))))

(defun xmlrpc-method-listchannels (args)
  *channels*)

(defparameter *xmlrpc-methods*
  '(("newpaste" xmlrpc-method-newpaste)
    ("pasteheaders" xmlrpc-method-pasteheaders)
    ("pasteheadersbychannel" xmlrpc-method-pasteheadersbychannel)
    ("pasteannotationheaders" xmlrpc-method-pasteannotationheaders)
    ("pastedetails" xmlrpc-method-pastedetails)
    ("listchannels" xmlrpc-method-listchannels)))

(defun xmlrpc-dispatch-method (method-name &rest args)
  (format t "Handling XML-RPC request for ~S from ~S~%" method-name s-xml-rpc:*xml-peer-address*)
  (handler-bind
      ((condition #'(lambda (c)
		      (return-from xmlrpc-dispatch-method
			(format nil "Error encountered: ~S" c)))))
    (if (find (ip-to-string s-xml-rpc:*xml-peer-address*)
	      *banned-ips* :test #'equal)
	"Naughty boy!"
	(let ((method (find method-name *xmlrpc-methods*
			    :test #'string-equal
			    :key #'car)))
	  (if method
	      (funcall (cadr method) args)
	      (format nil "Error: unimplemented method ~S." method-name))))))

(setf s-xml-rpc:*xml-rpc-call-hook* #'xmlrpc-dispatch-method)
