(defpackage :spec-lookup
  (:use :cl)
  (:export :lookup
           :*spec-directories*
           :read-specifications
           :term
           :key
           :url
           :term-type
           :title))

(in-package :spec-lookup)

(defvar *spec-directories*
  (list (asdf:system-relative-pathname :spec-lookup "specs/")))

(defvar *specs* ())

(defclass specification ()
  ((name :initarg :name
         :initform nil
         :accessor name)
   (description :initarg :description
                :initform nil
                :accessor description)
   (url-prefix :initarg :url-prefix
               :initform nil
               :accessor url-prefix)
   (terms :initarg :terms
          :initform nil
          :accessor terms)))

(defmethod print-object ((specification specification) stream)
  (print-unreadable-object (specification stream :type t :identity t)
    (princ (name specification) stream)))

(defclass term ()
  ((key :initarg :key
        :initform nil
        :accessor key)
   (url :initarg :url
        :initform nil
        :accessor url)
   (type :initarg :type
         :initform nil
         :accessor term-type)
   (title :initarg :title
          :initform nil
          :accessor title)))

(defmethod print-object ((term term) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (princ (key term) stream)))

(defun read-specifications (&key force)
  (when (or (not *specs*)
            force)
    (setf *specs*
          (loop for directory in *spec-directories*
                nconc
                (mapcar #'read-specification
                        (directory (merge-pathnames "*.lisp" directory))))))
  (values))

(defun wrapped-p (term char-set)
  (and (> (length term) 1)
       (char= (char term 0)
              (char term (1- (length term))))
       (find (char term 0) char-set)))

(defun abbrev (term &key (seperator #\-))
  (when (> (length term) 2)
    (let* ((wrap (wrapped-p term '(#\* #\+)))
           (term (if wrap
                     (subseq term 1 (1- (length term)))
                     term))
           (split (ppcre:split seperator term)))
      (when (and (> (length split) 1)
                 (every (lambda (e) (plusp (length e))) split))
        (format nil "~@[~a~]~{~c~^-~}~@[~a~]"
                wrap
                (mapcar (lambda (e)
                          (elt e 0)) split)
                wrap)))))

(defun parse-terms (terms &key abbreviate)
  (let ((hash-table (make-hash-table :test #'equalp)))
    (loop for term in terms
          do
          (destructuring-bind (key url &key type title) term
            (let ((term (make-instance 'term
                                       :key key
                                       :url url
                                       :type type
                                       :title title))
                  (abbreviation (and abbreviate
                                     (abbrev key))))
              (setf (gethash key hash-table) term)
              (when abbreviation
                (let ((existing (gethash abbreviation hash-table)))
                  (setf (gethash abbreviation hash-table)
                   (etypecase existing
                     (cons (cons term existing))
                     (null term)
                     (term (sort (list term existing)
                                 #'string> :key #'key)))))))))
    hash-table))

(defun read-data (file)
  (let (*read-eval*)
    (with-open-file (stream file)
      (read stream))))

(defun read-specification (file)
  (destructuring-bind ((&key name description url-prefix
                             abbreviate)
                       &rest terms)
      (read-data file)
    (make-instance 'specification
                   :name name
                   :description description
                   :url-prefix url-prefix
                   :terms (parse-terms terms :abbreviate abbreviate))))

(defun find-spec (name)
  (find name *specs* :key #'name :test #'equal))

(defun find-term (key spec)
  (gethash key (terms spec)))

(defun format-url (spec term)
  (if (consp term)
      (format nil "Matches: ~{~a~^, ~}."
              (mapcar #'key term))
      (format nil "~@[~a~]~a"
              (url-prefix spec)
              (url term))))

(defun lookup (spec term &key type)
  (let* ((spec (find-spec spec))
         (term (progn
                 (assert spec)
                 (find-term term spec))))
    (when term
      (values
       (format-url spec term)
       term))))
