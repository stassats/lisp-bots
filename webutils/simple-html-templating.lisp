(defpackage :webutils.simple-html-templating
  (:use :cl)
  (:export :deftag :let-tags :expand-template-body :load-template
           :recursively-process-current-template
           :capture-current-template :restore-template))
(in-package :webutils.simple-html-templating)
(webutils::export-all :webutils.simple-html-templating)

(defvar *tags-db* nil)

(defvar *remaining-template-body*)

(defmacro deftag (tag-name (output-stream-name &key (options (gensym))) &body forms)
  `(setf
    (getf *tags-db* ',tag-name)
    (lambda (,output-stream-name ,options)
      (declare (ignorable ,options))
      ,@forms)))

(defmacro let-tags (tag-definitions &body body &environment env)
  `(let ((*tags-db* *tags-db*))
     ,@(mapcar #'(lambda (tag-defn)
                   (macroexpand-1 `(deftag ,@tag-defn) env))
               tag-definitions)
     ,@body))

(defun recursively-process-current-template (stream)
  (loop while *remaining-template-body*
     for tag-or-string = (pop *remaining-template-body*)
     if (consp tag-or-string)
     do (let ((tag-expander (getf *tags-db* (car tag-or-string))))
          (if tag-expander
              (funcall tag-expander stream (cdr tag-or-string))
              (error "No tag expander for tag ~A!" (car tag-or-string))))
     else do (write-string tag-or-string stream)))

(defmacro capture-current-template (name &body body)
  `(let ((,name *remaining-template-body*))
     (unwind-protect
          (progn ,@body)
       (setf *remaining-template-body* ,name))))

(defmacro restore-template (name &body body)
  `(let ((*remaining-template-body* ,name))
     (unwind-protect
          (progn ,@body)
       (setf ,name *remaining-template-body*))))

(defun expand-template-body (template stream)
  (let ((*remaining-template-body* template))
    (recursively-process-current-template stream)))

(defparameter *tags-package* :keyword)

(defun read-tag-name (text start end)
  (let ((*package* (find-package *tags-package*))
        (*read-eval* nil))
    (read-from-string text t nil :start start :end end)))

(defun read-tag-options (text start end)
  (let ((*package* (find-package *tags-package*))
        (*read-eval* nil)
        (*readtable* (copy-readtable *readtable*)))
    (set-macro-character #\= (lambda (stream char)
                               (declare (ignore stream char))
                               (values)))
    (set-macro-character #\@ (lambda (stream char)
                               (declare (ignore stream char))
                               (error "@ may only be used to end a tag!")))
    (read-delimited-list #\@
                         (make-string-input-stream text start end))))

(defun load-template (pathname &key (tags-package :keyword))
  (let ((*tags-package* tags-package))
    (let ((template-text
	   (with-open-file (file pathname :direction :input)
	     (reduce #'(lambda (existing line)
			 (concatenate 'string existing
				      #.(format nil "~%")
				      line))
		     (loop for line = (read-line file nil nil)
			   while line
			   collect line)))))
      (loop with start-position = 0
	    with length = (length template-text)
	    while (< start-position length)
	    appending
	    (let* ((position-of-next-tag (search "<@" template-text :start2 start-position))
		   (end-of-tag (if position-of-next-tag
				   (search "@>" template-text :start2 (+ position-of-next-tag 2)))))
	      (if (and position-of-next-tag end-of-tag)
		  (prog1
		      (list (subseq template-text start-position position-of-next-tag)
			    (multiple-value-bind
				  (name-of-tag options-start)
				(read-tag-name template-text
					       (+ position-of-next-tag 2)
					       end-of-tag)
			      (let ((tag-options (read-tag-options template-text options-start (1+ end-of-tag))))
				(cons name-of-tag tag-options))))
		    (setf start-position (+ end-of-tag 2)))
		  (prog1
		      (list (subseq template-text start-position))
		    (setf start-position (1+ length)))))))))
