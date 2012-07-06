(defpackage :webutils.xml-mixed-mode (:use :cl :html-encode)
            (:export :install-xml-mixed-reader :uninstall-xml-mixed-reader
                     :with-xml :with-xml-output-to-stream :*xml-print-mode*
                     :xml-output-to-stream
                     :*break-xml-lines* :make-unescaped-string
                     :fail-match :fail-match-when :fail-match-unless
                     :define-translator :extend-translator :extend-translator*
                     :make-translator :apply-translator
                     :parse-xml-from-file :parse-xml-from-stream))
(in-package :webutils.xml-mixed-mode)
(webutils::export-all :webutils.xml-mixed-mode)

(defvar *xml-print-mode* :xml)
(defvar *break-xml-lines* t)
(defvar *depth* 0)

(defun xml-whitespacep (char)
  (or (eql char #\space)
      (eql char #\newline)
      (eql char #\linefeed)))

(defclass xml-tag () ((name :initarg :name)
                      (attributes :initarg :attributes)
                      (list-head :initarg :list-head)))

(defun make-xml-tag (name list-head attributes)
  (%make-xml-tag name list-head attributes))

(defun %make-xml-tag (name list-head attributes)
  (make-instance 'xml-tag :name name
                 :list-head list-head
                 :attributes attributes))

(define-compiler-macro make-xml-tag (&whole form name list-head attributes)
  (labels ((constantly-computable-attributes (attributes)
             (and (eq (first attributes) 'attribute)
                  (stringp (second attributes))
                  (stringp (third attributes))
                  (or (not (fourth attributes))
                      (constantly-computable-attributes (fourth attributes))))))
    (if (and (stringp name)
             (constantly-computable-attributes attributes))
        `(load-time-value (%make-xml-tag ,name ,list-head ,attributes) t)
        form)))

(defun attribute-append (attribute-list-form attributes)
  (append attribute-list-form attributes))

(defun attribute (name value further-attributes)
  (list* name value further-attributes))

(defclass unescaped-string () ((string :initarg :string)))
(defun make-unescaped-string (string)
  (make-instance 'unescaped-string :string string))

(defclass xml-tag-enclosed () ((tag :initarg :tag)
                               (object :initarg :object)))

(defun make-xml-tag-enclosed (tag object)
  (make-instance 'xml-tag-enclosed :tag tag :object object))

(defmacro string-equal-ecase (string &body cases)
  (let ((s (gensym)))
    `(let ((,s ,string))
       (cond
         ,@(loop for case in cases
                collect `((string-equal ,s ,(first case))
                          ,@(cdr case)))
         (t (error "~S did not match one of the expected cases: ~{~S~^ ~}"
                   ,s
                   ',(loop for case in cases collect (first case))))))))

(defun unescape-attribute-literal-string (string)
  (with-output-to-string (output)
    (with-input-from-string (input string)
      (flet ((read-entity ()
               (let ((complete-entity (with-output-to-string (entity)
                                        (loop for char = (read-char input)
                                           while (not (eql char #\;))
                                           do (write-char char entity)))))
                 (case (elt complete-entity 0)
                   (#\# (case (elt complete-entity 1)
                          (#\x (code-char (parse-integer complete-entity :start 2 :radix 16)))
                          (t (code-char (parse-integer complete-entity :start 1 :radix 10)))))
                   (t (string-equal-ecase complete-entity
                        ("amp" #\&)
                        ("quot" #\")
                        ("apos" #\')
                        ("lt" #\<)
                        ("gt" #\>)
                        ("nbsp" (code-char #xA0))))))))
       (loop for char = (read-char input nil nil)
          while char
          if (eql char #\&)
          do (write-char (read-entity) output)
          else do (write-char char output))))))

(defun print-xml-tag (tag stream)
  (write-char #\< stream)
  (write-string (slot-value tag 'name) stream)
  (when (slot-value tag 'attributes)
    (write-char #\space stream))
  (loop for (attribute value) on (slot-value tag 'attributes) by #'cddr
     do (if (typep attribute 'unescaped-string)
            (write-string (slot-value attribute 'string) stream)
            (write-string (argument-encode attribute) stream))
     do (write-char #\= stream)
     do (write-char #\" stream)
     do (if (typep value 'unescaped-string)
            (write-string (slot-value value 'string) stream)
            (write-string (argument-encode value) stream))
     do (write-char #\" stream)
     do (write-char #\space stream))
  (if (slot-value tag 'list-head)
      (progn
        (when *break-xml-lines*
          (terpri stream)
          (dotimes (i *depth*)
            (write-char #\space stream)))
        (write-char #\> stream))
      (ecase *xml-print-mode*
        (:xml (write-char #\/ stream)
              (write-char #\> stream))
        (:html (write-char #\> stream))
        (:empty-body (write-char #\> stream)
                     (print-closing-xml-tag tag stream :newline nil)))))

(defun print-closing-xml-tag (tag stream &key (newline t))
  (write-char #\< stream)
  (write-char #\/ stream)
  (write-string (slot-value tag 'name) stream)
  (when (and newline *break-xml-lines*)
    (terpri stream)
    (dotimes (i *depth*)
      (write-char #\space stream)))
  (write-char #\> stream))

(defun print-xml-object (object stream)
  (etypecase object
    (xml-tag
     (print-xml-tag object stream))
    (xml-tag-enclosed
     (print-xml-tag (slot-value object 'tag) stream)
     (let ((*depth* (1+ *depth*)))
      (print-xml-object (slot-value object 'object) stream))
     (print-closing-xml-tag (slot-value object 'tag) stream))
    (list
     (loop for element in object
        do (print-xml-object element stream)))
    (string
     (write-string (encode-for-pre object) stream))
    (unescaped-string
     (write-string (slot-value object 'string) stream))
    (integer (prin1 object stream))))

(defun argument-encode (argument)
  (if (typep argument 'unescaped-string)
      argument
      (encode-for-argument (string argument))))

(defun read-from-string-until (string until-char &key (start 0) end)
  (if (get-macro-character (elt string start))
      (read-from-string string t nil :start start :end end)
      (let ((*readtable* (copy-readtable *readtable*)))
        (set-macro-character until-char (lambda (stream char)
                                          (declare (ignore stream char))
                                          (values)))
        (read-from-string string t nil :start start :end end))))

(defun parse-tag (tag)
  (assert (eql (elt tag 0) #\<))
  (assert (eql (elt tag (1- (length tag))) #\>)) 
  (let* ((is-list-head (not (eql (elt tag (- (length tag) 2)) #\/)))
         (without-anglies (subseq tag 1 (- (length tag)
                                           (if is-list-head 1 2)))) 
         tag-name
         (current-position 0))
    (if (eql (elt without-anglies 0) #\?)
        (multiple-value-bind
              (value new-position)
            (read-from-string without-anglies t nil :start (1+ current-position))
          (setf tag-name value
                current-position new-position))
        (let ((tag-name-end (or (position-if #'(lambda (char)
                                            (or (eql char #\space)
                                                (eql char #\newline)
                                                (eql char #\linefeed)))
                                        without-anglies)
                           (length without-anglies))))
          (setf tag-name (subseq without-anglies current-position tag-name-end)
                current-position tag-name-end)))
    (labels ((at-end () (eql current-position (length without-anglies)))
             (current-position ()
               (elt without-anglies current-position))
             (skip-whitespace ()
               (loop while (and (not (at-end))
                                (xml-whitespacep (current-position)))
                  do (incf current-position)))
             (read-attributes ()
               (skip-whitespace)
               (let (attribute-name attribute-value attributes-compute-form attributes-compute-form-found)
                 (unless (at-end)
                   (case (current-position)
                     (#\? ;; computed attribute name
                      (incf current-position)
                      (skip-whitespace)
                      (multiple-value-bind
                            (value new-position)
                          (read-from-string-until without-anglies #\= :start current-position)
                        (setf attribute-name value
                              current-position new-position)))
                     (#\$ ;; computed attributes
                      (incf current-position)
                      (multiple-value-bind
                            (value new-position)
                          (read-from-string without-anglies t nil :start current-position)
                        (setf attributes-compute-form value
                              attributes-compute-form-found t
                              current-position new-position)))
                     (t
                      (let ((start current-position))
                        (loop while (not (or (xml-whitespacep (current-position))
                                             (eql (current-position) #\=)))
                           do (incf current-position))
                        (setf attribute-name
                              (unescape-attribute-literal-string (subseq without-anglies start current-position))))))
                   (unless attributes-compute-form-found
                     (skip-whitespace)
                     (assert (eql (current-position) #\=))
                     (incf current-position)
                     (skip-whitespace)
                     (ecase (current-position)
                       (#\? ;; computed attribute value
                        (incf current-position)
                        (multiple-value-bind
                              (value new-position)
                            (read-from-string without-anglies t nil :start current-position)
                          (setf attribute-value value
                                current-position new-position)))
                       ((#\" #\')
                        (let ((start current-position))
                          (incf current-position)
                          (loop while (not (or (at-end)
                                               (eql (current-position)
                                                    (elt without-anglies start))))
                             do (incf current-position))
                          (incf current-position)
                          (setf attribute-value
                                (unescape-attribute-literal-string (subseq without-anglies (1+ start)
                                                                           (1- current-position))))))))
                   (if attributes-compute-form-found
                       `(attribute-append ,attributes-compute-form
                                          ,(read-attributes))
                       `(attribute ,attribute-name ,attribute-value
                                   ,(read-attributes)))))))
      (let ((tag-constructor-form `(make-xml-tag ,tag-name ,(if is-list-head t nil)
                                                 ,(read-attributes))))
        (if is-list-head
            (let ((g (gensym)))
              `(lambda (&rest ,g)
                 (make-xml-tag-enclosed ,tag-constructor-form ,g)))
            tag-constructor-form)))))

(defmacro with-xml (&body lexical-forms)
  `(progn ,@lexical-forms))

(defmacro with-xml-output-to-stream (stream &body lexical-forms)
  (let ((g (gensym)))
    `(let ((,g ,stream))
       ,@(loop for form in lexical-forms
            collect `(print-xml-object ,form ,g))
       t)))

(defun xml-output-to-stream (stream xml-object)
  (print-xml-object xml-object stream))

(defun xml-<-read (stream char)
  (declare (ignore char))
  (let ((char (peek-char nil stream nil #\space t)))
    (cond
      ((and (eql char #\=)
            (find-symbol "<=" *package*))
       (read-char stream t nil t)
       (intern "<=" *package*))
      ((xml-whitespacep char)
       (intern "<" *package*))
      (t
       (parse-tag
        (with-output-to-string (s)
          (write-char #\< s)
          (loop for c = (read-char stream t nil t)
             while (not (eql c #\>))
             do (write-char c s))
          (write-char #\> s)))))))

(defun install-xml-mixed-reader ()
  (set-macro-character #\< 'xml-<-read t))

(defun uninstall-xml-mixed-reader ()
  (set-syntax-from-char #\< #\>))

(install-xml-mixed-reader)
