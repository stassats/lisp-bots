(defpackage :spec-lookup
  (:use :cl)
  (:export :lookup
           :*spec-directories*
           :read-specifications
           :term
           :key
           :url
           :term-type
           :title
           :*specs*
           :specification
           :name
           :description
           :url-prefix
           :terms
           :validator
           :processor))

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
          :accessor terms)
   (validator :initarg :validator
              :initform
              (lambda (string)
                (not (find #\Space string)))
              :accessor validator)
   (processor :initarg :processor
              :initform nil
              :accessor processor)))

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

(defun join-key-to-hash-table (key value hash-table)
  (let ((existing (gethash key hash-table)))
    (setf (gethash key hash-table)
          (etypecase existing
            (cons (sort (cons value existing)
                        #'string< :key #'key))
            (null value)
            (term (sort (list value existing)
                        #'string< :key #'key))))))

(defun add-abbreviated-forms (key term hash-table)
  (let ((abbreviation (abbrev key)))
    (when abbreviation
      (join-key-to-hash-table abbreviation term hash-table))))

(defun parse-terms (terms &key abbreviate)
  (let ((hash-table (make-hash-table :test #'equalp)))
    (loop for term in terms
          do
          (destructuring-bind (key url &key type title) term
            (let* ((typed (if type
                              (format nil "~a/~(~a~)" key type)
                              key))
                   (term (make-instance 'term
                                        :key typed
                                        :url url
                                        :type type
                                        :title title)))
              (join-key-to-hash-table key term hash-table)
              (when type
                (setf (gethash typed hash-table) term))
              (when abbreviate
                (add-abbreviated-forms key term hash-table)))))
    hash-table))

(defun read-data (file)
  (let (*read-eval*)
    (with-open-file (stream file)
      (read stream))))

(defun read-specification (file)
  (destructuring-bind ((&key name description url-prefix
                             abbreviate
                             validator
                             processor)
                       &rest terms)
      (read-data file)
    (make-instance 'specification
                   :name name
                   :description description
                   :url-prefix url-prefix
                   :validator (if validator
                                  (compile nil validator)
                                  (constantly t))
                   :processor (and processor
                                   (coerce processor 'function))
                   :terms (parse-terms terms :abbreviate abbreviate))))

(defun find-spec (name)
  (find name *specs* :key #'name :test #'equal))

(defun find-term (key spec)
  (if (processor spec)
      (funcall (processor spec) key)
      (gethash key (terms spec))))

(defun format-url (spec term)
  (if (consp term)
      (format nil "Matches: ~{~a~^, ~}."
              (mapcar #'key term))
      (format nil "~@[~a~]~a"
              (url-prefix spec)
              (if (stringp term)
                  term
                  (url term)))))

(defun lookup (spec-designator term)
  (let* ((spec (if (typep spec-designator 'specification)
                   spec-designator
                   (find-spec spec-designator)))
         (valid (progn (assert spec)
                       (funcall (validator spec) term)))
         (term (and valid
                    (find-term term spec))))
    (if term
        (values (format-url spec term) term)
        (values nil valid))))
