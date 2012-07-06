(defpackage :webutils.forms (:use :cl :araneida :split-sequence :html-encode :webutils.simple-serialized-classes :webutils.xml-mixed-mode)
            (:export :with-processed-form
                     :form :form-field :textarea-form-field
                     :hidden-form-field :immutable-form-field
                     :selector-form-field :boolean-form-field :resubmit-form
                     :form-html :define-form :define-template-form-field
                     :define-class-gate-template-form-field
                     :define-class-gate-form-field
                     :keyword->pretty-string :pretty-string->keyword
                     :nonempty-string-validator :password-form-field
                     :submit-ecase :fail-check :define-form-field
                     :define-form-field-length-constraint
                     :form-get-url :call-parent-method :get-error-message
                     :define-optional-template :set-slots-from-form-values
                     :multiple-form-block))
(defpackage :webutils.form-cookies (:use))
(in-package :webutils.forms)
(webutils::export-all :webutils.forms)

(defun protect-for-dom (string)
  (substitute #\_ #\- string))

(defvar *failed-acceptor-form-catch-tag* (gensym))

(defmacro fail-check (reason &rest offending)
  `(throw *failed-acceptor-form-catch-tag*
     (values nil ,reason ',offending)))

(defun translate-length-constraint-into-words (constraint value field &key invert)
  (ecase (first constraint)
    ((< > <= >= =)
     (format nil "The length of the ~(~A~) should ~A ~A ~A."
             field
             (if invert "not be" "be")
             (case (first constraint)
               (< "less than")
               (> "greater than")
               (<= "less than or equal to")
               (>= "greater than or equal to")
               (= "equal to")) (second constraint)))
    (and
     (if invert
         (translate-length-constraint-into-words
          `(or ,@(mapcar (lambda (constraint)
                           `(not ,constraint)) (cdr constraint))) value field :invert nil)
         (translate-length-constraint-into-words
              (find-failing-constraint constraint value)
              value field)))
    (or
     (if invert
         (translate-length-constraint-into-words
          `(and ,@(mapcar (lambda (constraint)
                            `(not ,constraint)) (cdr constraint))) value field :invert nil)
         (format nil "None of the following were true: ~{~A~^ ~}"
                 (mapcar (lambda (constraint)
                           (translate-length-constraint-into-words constraint value field :invert invert))
                         (cdr constraint)))))
    (not (translate-length-constraint-into-words (second constraint) value field :invert (not invert)))))

(defun find-failing-constraint (constraint-expr value)
  (case (first constraint-expr)
    (and (some (lambda (constraint)
                 (find-failing-constraint constraint value)) (cdr constraint-expr)))
    (or (unless (not (every (lambda (constraint)
                              (find-failing-constraint constraint value)) (cdr constraint-expr)))
          constraint-expr))
    (not (unless (find-failing-constraint (second constraint-expr) value)
           constraint-expr))
    (t (unless (funcall (first constraint-expr)
                        value
                        (second constraint-expr))
         constraint-expr))))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun translate-length-constraint (constraint var)
   (labels ((malformed ()
              (error "The constraint ~S is malformed." constraint)))
     (ecase (first constraint)
       ((< > <= >= =)
        (unless (and (eql (length constraint) 2)
                     (numberp (second constraint)))
          (malformed))
        `(,(first constraint) ,var ,(second constraint)))
       (not (unless (eql (length constraint) 2)
              (malformed))
            `(not ,(translate-length-constraint (second constraint) var)))
       ((and or)
        `(,(first constraint) ,@(mapcar (lambda (constraint)
                                          (translate-length-constraint constraint var))
                                        (cdr constraint))))))))

(defun translate-max-constraint (constraint)
  (case (first constraint)
    (< (1- (second constraint)))
    ((<= =) (second constraint))))

(defvar *forms* nil)
(defvar *template-form-fields* nil)

(defmacro define-template-form-field (template-name class &body arguments)
  (setf *template-form-fields* (remove template-name *template-form-fields* :key #'car))
  (let ((found (find class *template-form-fields* :key #'first)))
    (if found
        (push (list* template-name (second found)
                     (append
                      (loop for (key lambda) on arguments by #'cddr
                           with argument = (gensym)
                         collect key
                         collect `(flet ((call-parent-method (,argument)
                                           (funcall
                                            ,(getf (cddr found) key '#'identity)
                                            ,argument)))
                                    ,lambda))
                      (loop for (key lambda) on (cddr found) by #'cddr
                         unless (getf arguments key)
                         collect key
                         unless (getf arguments key)
                         collect lambda))) *template-form-fields*)
        (push (list* template-name class arguments) *template-form-fields*)))
  (values))

(defmacro define-optional-template (template-name class)
  `(define-template-form-field ,template-name ,class
     :string-acceptor (lambda (string)
                        (if (zerop (length string))
                            nil
                            (call-parent-method string)))
     :string-to-value-translator (lambda (string)
                                   (if (zerop (length string))
                                       nil
                                       (call-parent-method string)))
     :value-to-string-translator (lambda (string)
                                   (if string (call-parent-method string) ""))))

(defmacro define-class-gate-template-form-field (template-name form-field-class class &key (key-slot-type 'string))
  `(define-template-form-field ,template-name ,form-field-class ,@(gate-initargs form-field-class class key-slot-type)))

(define-condition form-generation-error (serious-condition)
  ((reason :initform "Unknown" :accessor form-generation-error-reason :initarg :reason)))

(defclass form ()
  ((name :initarg :name :accessor form-name)
   (fields :initform nil :initarg :fields :accessor form-fields)
   (semantic-checks :initform nil :initarg :semantic-checks :accessor form-semantic-checks)
   (inherit-forms :initform nil :initarg :inherit-forms :accessor form-inherit-forms)
   (expected-fields :initform nil :initarg :expected-fields :accessor form-expected-fields)
   (submit-text-mappings :initform nil :initarg :submit-text-mappings :accessor form-submit-text-mappings)
   (cookie :initform (gentemp "COOKIE" (find-package :webutils.form-cookies)) :initarg :cookie :accessor form-cookie)))

(defmethod print-object ((object form) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (form-name object) stream)))

(defun find-form (form-name)
  (find form-name *forms* :key #'form-name))

(defun (setf find-form) (new-value form-name)
  (car (setf *forms* (cons new-value (remove form-name *forms* :key #'form-name)))))

(defun situation-intersect (situation1 situation2)
  (or (find t situation2)
      (find t situation1)
      (intersection situation1 situation2)))

(defun specifier-active (designator1 designator2)
  (and (eq (car designator1) (car designator2))
       (situation-intersect (cdr designator1) (cdr designator2))))

(defun find-form-field (form-name specifier)
  (labels ((%find (form)
             (let ((found (find specifier (form-fields form) :key (lambda (field)
                                                                    (cons (form-field-name field)
                                                                          (form-field-situations field)))
                                :test #'specifier-active)))
               (or found
                   (loop for (form . situations) in (form-inherit-forms form)
                      for val = nil
                      if (and (situation-intersect (cdr specifier) situations)
                              (setf val (%find (find-form form))))
                      do (return val))))))
    (%find (or (find-form form-name) (error "No such form ~A" form-name)))))

(defun (setf find-form-field) (new-value form-name specifier)
  (let ((form (or (find-form form-name) (error "No such form ~A" form-name))))
    (labels ((%find (form)
               (let ((found (find specifier (form-expected-fields form) :test #'specifier-active)))
                     (or found
                         (loop for (form . situations) in (form-inherit-forms form)
                            for val = nil
                            if (and (situation-intersect (cdr specifier) situations)
                                    (setf val (%find (find-form form))))
                            do (return val))))))
      (unless (%find form)
        (error "The field ~A is not expected in the form ~A." specifier form-name)))
    (car (setf (form-fields form)
               (cons new-value (remove specifier (form-fields form) :key (lambda (field)
                                                                           (cons (form-field-name field)
                                                                                 (form-field-situations field))) :test #'equal))))))

(defmacro form-field (form-name &rest specifier)
  `(find-form-field ',form-name ',specifier))

(defun situation-specificity (sit1 sit2)
  (reduce #'+ (mapcar (lambda (situation)
                        (count situation sit2)) sit1)))

(defun union-form-fields (fields)
  (remove-duplicates fields :key #'form-field-name :from-end t))

(defun union-form-field-situations/names (fields)
  (remove-duplicates fields :key #'car :from-end t))

(defun active-form-fields (form situations)
  (union-form-fields
   (nconc (loop for field in (form-fields form)
             if (or (find t (form-field-situations field))
                    (situation-intersect (form-field-situations field) situations))
             collect field)
          (loop for (inherit . inherit-situations) in (form-inherit-forms form)
               if (situation-intersect inherit-situations situations)
             nconc (active-form-fields (find-form inherit) situations)))))

(defun active-form-field-situations/names (form situations)
  (union-form-field-situations/names
   (nconc (loop for field in (form-expected-fields form)
             if (or (find t (cdr field))
                    (situation-intersect (cdr field) situations))
             collect field)
          (loop for (inherit . inherit-situations) in (form-inherit-forms form)
             if (situation-intersect situations inherit-situations)
             nconc (active-form-field-situations/names (find-form inherit) situations)))))

(defun active-form-field-names (form situations)
  (mapcar #'car (active-form-field-situations/names form situations)))

(defun active-form-fields-in-correct-order (form situations)
  (let ((active-field-names (active-form-field-names form situations))
        (active-fields (active-form-fields form situations)))
    (mapcar (lambda (name)
              (find name active-fields :key #'form-field-name)) active-field-names)))

(defun active-form-semantic-checks (form situations)
  (nconc (loop for (inherit . inherit-situations) in (form-inherit-forms form)
            if (situation-intersect situations inherit-situations)
            append (active-form-semantic-checks (find-form inherit) situations))
         (loop for check in (form-semantic-checks form)
            if (or (null (car check))
                   (situation-intersect (car check) situations))
            collect check)))

(defun active-form-semantic-checks/variables (form situations)
  (nconc (loop for (inherit . inherit-situations) in (form-inherit-forms form)
            if (situation-intersect situations inherit-situations)
            append (active-form-semantic-checks/variables (find-form inherit) situations))
         (loop for check in (form-semantic-checks form)
            if (or (null (car check))
                   (situation-intersect (car check) situations))
            collect (cons (active-form-field-names form (car check))
			  check))))

(defun make-semantic-check-lambda-form (variables semantic-check-body)
  (let ((block-name (gensym))
        (reason (gensym))
        (offending (gensym)))
   `(macrolet ((fail-check (,reason &rest ,offending)
                 `(return-from ,',block-name (values nil ,,reason ',,offending))))
      (lambda (,@variables)
        (declare (ignorable ,@variables))
        (block ,block-name
          ,@semantic-check-body
          t)))))

(defmacro define-form (form-name (&rest inherit-forms) (&rest expected-fields) &body options)
  (flet ((%fix (thing)
           (if (symbolp thing)
               (list thing t)
               thing)))
   (let ((semantic-checks (remove :semantic-check options :key #'car :test-not #'eq))
         (submit-text-mappings (remove :submit-text options :key #'car :test-not #'eq))
         (cookie (gentemp "COOKIE" (find-package :webutils.form-cookies)))
         (inherit-forms (mapcar #'%fix inherit-forms))
         (expected-fields (mapcar #'%fix expected-fields)))
     (mapc (lambda (form-name-and-situation)
             (unless (find-form (car form-name-and-situation))
               (error "Inherited form ~A has not yet been defined." (car form-name-and-situation))))
           inherit-forms)
     (setf (find-form form-name)
           (make-instance 'form
                          :name form-name
                          :inherit-forms inherit-forms
                          :expected-fields expected-fields
                          :cookie cookie))
     `(setf (find-form ',form-name)
            (make-instance 'form
                           :name ',form-name
                           :fields nil
                           :expected-fields ',expected-fields
                           :inherit-forms ',inherit-forms
                           :submit-text-mappings
                           (list ,@(mapcar (lambda (submit-text)
                                             `(cons ',(butlast (cdr submit-text))
                                                    ,(car (last submit-text))))
                                           submit-text-mappings))
                           :semantic-checks
                           (list ,@(loop for check-option in semantic-checks
                                      appending
                                        (multiple-value-bind
                                              (situations body)
                                            (loop for (possible-situation . rest) on (cdr check-option)
                                               while (keywordp possible-situation)
                                               collect possible-situation into situations
                                               finally (return (values situations (cons possible-situation rest))))
                                          (let ((situations (or situations '(t))))
                                            (loop for situation in situations
                                                  collect
                                                  `(cons ',(list situation)
                                                    ,(make-semantic-check-lambda-form
                                                      (active-form-field-names (find-form form-name) (list situation))
                                                      body)))))))
             :cookie ',cookie)))))

(defmacro define-form-field ((form-name name &rest situations) class &body arguments)
  (let ((template (assoc class *template-form-fields*))
        (situations (or situations '(t))))
    (when (and template arguments)
      (warn "Warning: template form field ~A found; arguments will be ignored." class))
    (if template
        `(setf (form-field ,form-name ,name ,@situations)
               (make-instance ',(second template)
                              :name ',name
                              :situations ',situations
                              ,@(cddr template)))
        `(setf (form-field ,form-name ,name ,@situations)
               (make-instance ',class
                              :name ',name
                              :situations ',situations
                              ,@arguments)))))

(defmacro define-form-field-length-constraint ((form-name name &rest situations) &body constraint-body)
  (let ((g (make-symbol "VALUE"))
        (p (make-symbol "PRETTY-NAME"))
        (constraint (if (eql (length constraint-body) 1)
                        (first constraint-body)
                        `(and ,@constraint-body))))
   `(setf (form-field-length-constraint (form-field ,form-name ,name ,@situations))
          (lambda (,g ,p)
            (unless
                ,(translate-length-constraint constraint g)
              (fail-check (translate-length-constraint-into-words ',constraint ,g ,p))))
          (form-field-max-length (form-field ,form-name ,name ,@situations))
          ,(translate-max-constraint constraint))))

(defmacro define-class-gate-form-field ((form-name name &rest situations) form-field-class class &key (key-slot-type 'string))
  `(define-form-field (,form-name ,name ,@situations) ,form-field-class ,@(gate-initargs form-field-class class key-slot-type)))

(defun keyword->pretty-string (keyword)
  (let ((split (split-sequence #\- (symbol-name keyword))))
    (when (and (> (length split) 1)
               (equal (first (last split)) "P"))
      (setf split (nconc (butlast split 2)
                         (list
                          (concatenate 'string (first (last split 2)) "?")))))
    (format nil "~@(~{~A~^ ~}~)" split)))

(defun pretty-string->keyword (string)
  (intern (format nil "~:@(~{~A~^-~}~)"
                  (split-sequence #\space string)) :keyword))

(defgeneric form-field-in-table-p (field))

(defgeneric form-field-accepting-html (field initial-value))

(defgeneric gate-initargs (form-class class-name key-type))

(defclass form-field ()
  ((name :initarg :name :accessor form-field-name)
   (situations :initarg :situations :accessor form-field-situations)
   (pretty-name :initarg :pretty-name :accessor form-field-pretty-name)
   (string-acceptor :initarg :string-acceptor :accessor form-field-string-acceptor :initform (constantly t))
   (string-to-value-translator :initarg :string-to-value-translator :accessor form-field-string-to-value-translator :initform #'identity)
   (value-to-string-translator :initarg :value-to-string-translator :accessor form-field-value-to-string-translator :initform #'identity)
   (default-value :initarg :default-value :accessor form-field-default-value :initform nil)
   (length-constraint :initarg :length-constraint :accessor form-field-length-constraint :initform (constantly t))
   (max-length :initarg :max-length :accessor form-field-max-length :initform nil)))

(defmethod gate-initargs ((form-class (eql 'form-field)) class-name key-type)
  `(:string-acceptor
    (lambda (string)
      ,@(ecase key-type
               (string nil)
               (integer `((unless (parse-integer string :junk-allowed t)
                            (fail-check "Please enter an integer.")))))
      (let ((key ,(ecase key-type (string 'string) (integer '(parse-integer string)))))
       (unless (find-instance-by-key key ',class-name)
         (fail-check (format nil "Please enter a valid ~(~A~)." ',class-name)))))
    :string-to-value-translator (lambda (string)
                                  (let ((key ,(ecase key-type (string 'string) (integer '(parse-integer string)))))
                                   (find-instance-by-key key ',class-name)))
    :value-to-string-translator ,(ecase key-type
                                        (string '#'key-of-instance)
                                        (integer '(lambda (value)
                                                   (prin1-to-string (key-of-instance value)))))))

(defmethod print-object ((object form-field) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (form-field-name object) stream)))

(defclass protected-string ()
  ((string :initarg :string :accessor protected-string-string)))

(defmacro protected-string (string)
  `(make-instance 'protected-string :string (or ,string "")))

(defmethod form-field-in-table-p ((field form-field))
  t)

(defmethod form-field-accepting-html ((field form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (if initial-value
                   (funcall (form-field-value-to-string-translator field) initial-value)
                   "")))
    <input type= "text" name=?name value=?value $(if (form-field-max-length field)
                                                     (list "maxlength" (princ-to-string (form-field-max-length field)))) />))

(defmethod form-field-accepting-html ((field form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (protected-string-string initial-value)))
    <input type= "text" name=?name value=?value $(if (form-field-max-length field)
                                                     (list "maxlength" (princ-to-string (form-field-max-length field))))/>))

(defmethod initialize-instance :after ((instance form-field) &rest slots &key &allow-other-keys)
  (declare (ignore slots))
  (if (some #'(lambda (e)
                (not (or (alphanumericp e)
                         (member e '(#\- #\_ #\.)))))
            (symbol-name (form-field-name instance)))
      (error "~S is a bad name for a form field; I don't know that it can be reliably used for a field name in HTML." (form-field-name instance)))
  (if (not (slot-boundp instance 'pretty-name))
      (setf (form-field-pretty-name instance)
            (keyword->pretty-string (form-field-name instance)))))

(defclass password-form-field (form-field)
  ())

(defmethod gate-initargs ((form-class (eql 'password-form-field)) class-name key-type)
  (gate-initargs 'form-field class-name key-type))

(defmethod form-field-accepting-html ((field password-form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (if initial-value
                        (funcall (form-field-value-to-string-translator field) initial-value)
                        "")))
    <input type= "password" name=?name value=?value $(if (form-field-max-length field)
                                                         (list "maxlength" (princ-to-string (form-field-max-length field)))) />))

(defmethod form-field-accepting-html ((field password-form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (protected-string-string initial-value)))
    <input type= "password" name=?name value=?value $(if (form-field-max-length field)
                                                         (list "maxlength" (princ-to-string (form-field-max-length field))))/>))

(defclass textarea-form-field (form-field)
  ((rows :initarg :rows :initform 24 :accessor textarea-form-field-rows)
   (cols :initarg :cols :initform 40 :accessor textarea-form-field-cols)))

(defmethod gate-initargs ((form-class (eql 'textarea-form-field)) class-name key-type)
  (gate-initargs 'form-field class-name key-type))

(defmethod form-field-accepting-html ((field textarea-form-field) initial-value)
  (let ((rows (princ-to-string (textarea-form-field-rows field)))
        (cols (princ-to-string (textarea-form-field-cols field)))
        (name (protect-for-dom (symbol-name (form-field-name field)))))
    (<textarea rows=?rows cols=?cols name=?name>
               (if initial-value
                   (funcall (form-field-value-to-string-translator field) initial-value)
                   ""))))

(defmethod form-field-accepting-html ((field textarea-form-field) (initial-value protected-string))
  (let ((rows (princ-to-string (textarea-form-field-rows field)))
        (cols (princ-to-string (textarea-form-field-cols field)))
        (name (protect-for-dom (symbol-name (form-field-name field)))))
    (<textarea rows=?rows cols=?cols name=?name>
               (protected-string-string initial-value))))

(defclass hidden-form-field (form-field) ())

(defmethod gate-initargs ((form-class (eql 'hidden-form-field)) class-name key-type)
  (gate-initargs 'form-field class-name key-type))

(defmethod form-field-in-table-p ((field hidden-form-field))
  nil)

(defmethod form-field-accepting-html ((field hidden-form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (if initial-value
                        (funcall (form-field-value-to-string-translator field) initial-value)
                        "")))
    <input type= "hidden" name=?name value=?value />))

(defmethod form-field-accepting-html ((field hidden-form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (protected-string-string initial-value)))
    <input type= "hidden" name=?name value=?value />))

(defclass immutable-form-field (form-field) ())

(defmethod gate-initargs ((form-class (eql 'immutable-form-field)) class-name key-type)
  (gate-initargs 'form-field class-name key-type))

(defmethod form-field-in-table-p ((field immutable-form-field))
  t)

(defmethod form-field-accepting-html ((field immutable-form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (if initial-value
                    (funcall (form-field-value-to-string-translator field) initial-value)
                    "")))
    (<span>
       <input type= "hidden" name=?name value=?value />
       value)))

(defmethod form-field-accepting-html ((field immutable-form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name field))))
        (value (protected-string-string initial-value)))
    (<span>
       <input type= "hidden" name=?name value=?value />
       value)))

(defclass selector-form-field (form-field)
  ((allowed-values-generator :initarg :allowed-values-generator :accessor selector-form-field-allowed-values-generator)))

(defmethod gate-initargs ((form-class (eql 'selector-form-field)) class-name key-type)
  (list* :allowed-values-generator
         `(lambda ()
           (store-of-class ,class-name)) (gate-initargs 'form-field class-name key-type)))

(defmethod form-field-accepting-html ((form-field selector-form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name form-field))))
        (allowed (funcall (selector-form-field-allowed-values-generator form-field))))
    (if (not allowed)
        (signal 'form-generation-error :reason (format nil "No values were found for the field ~A" (form-field-pretty-name form-field)))
        (<select name=?name>
                 (loop for value in allowed
                    for selected = (if (eq value initial-value) '("selected" "SELECTED"))
                    collect (<option $selected>
                                     (funcall (form-field-value-to-string-translator form-field) value)))))))

(defmethod form-field-accepting-html ((form-field selector-form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name form-field))))
        (allowed (funcall (selector-form-field-allowed-values-generator form-field))))
    (if (not allowed)
        (signal 'form-generation-error :reason (format nil "No values were found for the field ~A" (form-field-pretty-name form-field)))
        (<select name=?name>
                   (loop for value in allowed
                      for stringified = (funcall (form-field-value-to-string-translator form-field) value)
                      for selected = (if (equal stringified (protected-string-string initial-value)) '("selected" "SELECTED"))
                        collect (<option $selected>
                                         stringified))))))

(defmethod initialize-instance :after ((instance selector-form-field) &rest slots &key &allow-other-keys)
  (declare (ignore slots))
  (setf (form-field-string-acceptor instance)
        (lambda (value)
          (let ((allowed (funcall (selector-form-field-allowed-values-generator instance))))
            (if (member value allowed :test #'equalp :key (form-field-value-to-string-translator instance))
                t
                (values nil "Please choose an allowed value."))))))

(defclass boolean-form-field (form-field)
  ())

(defmethod form-field-accepting-html ((form-field boolean-form-field) initial-value)
  (let ((name (protect-for-dom (symbol-name (form-field-name form-field))))
        (checked (if initial-value '("checked" "CHECKED"))))
    <input type= "checkbox" name=?name $checked />))

(defmethod form-field-accepting-html ((form-field boolean-form-field) (initial-value protected-string))
  (let ((name (protect-for-dom (symbol-name (form-field-name form-field))))
        (checked (if (not (string= (protected-string-string initial-value) "")) '("checked" "CHECKED"))))
    <input type= "checkbox" name=?name $checked />))

(defmethod initialize-instance :after ((instance boolean-form-field) &rest slots &key &allow-other-keys)
  (declare (ignore slots))
  (setf (form-field-string-to-value-translator instance)
        (lambda (string)
          (if (> (length string) 0) t nil)))
  (setf (form-field-value-to-string-translator instance)
        (lambda (value)
          (if value "CHECKED" ""))))

(defun generate-form-table-rows (field-and-initial-value-pairs offending-fields)
  (loop for (form-field initial-value) in field-and-initial-value-pairs
      for alternate = nil then (not alternate)
      for alternate-attribute = (if (member (form-field-name form-field) offending-fields)
                                    '("class" "offending")
                                    (if alternate '("class" "alternate")))
      if (form-field-in-table-p form-field)
      collect (<tr $alternate-attribute>
                   (<th> (form-field-pretty-name form-field))
                   (<td> (form-field-accepting-html form-field initial-value)))
      else do (setf alternate (not alternate))))

(defun generate-hidden-form-table-rows (field-and-initial-value-pairs)
  (loop for (form-field initial-value) in field-and-initial-value-pairs
       if (not (form-field-in-table-p form-field))
       collect (form-field-accepting-html form-field initial-value)))

(defun url-string-or-urlstring (url)
  (etypecase url
    (string url)
    (url (urlstring url))))

(defmacro multiple-form-block-expand-focus-logic () t)
(defmacro multiple-form-block (&body arguments)
  (let ((g (gensym)))
   `(symbol-macrolet ((,g ,(list t)))
      (macrolet ((multiple-form-block-expand-focus-logic (&environment env)
                   (let ((expansion (macroexpand-1 ',g env)))
                     (prog1 (car expansion)
                       (setf (car expansion) nil)))))
        (list ,@arguments)))))

(defun generate-form (form-name processing-url submit-text method table-class field-and-initial-value-pairs error-text offending-fields &key expand-focus-logic)
  (let ((submit-text (if (atom submit-text)
                         (list submit-text)
                         submit-text))
        (action (url-string-or-urlstring processing-url))
        (form-name (protect-for-dom form-name)))
    (handler-case
         (<form method=?method action=?action name=?form-name>
                (if error-text
                    (<span class= "form-error">
                           error-text))
                (<table class=?table-class>
                        (generate-form-table-rows field-and-initial-value-pairs offending-fields)
                        (<tr>
                         <th/>
                         (<td>
                          (generate-hidden-form-table-rows field-and-initial-value-pairs)
                          (loop for submit in submit-text
                             for value = (if (keywordp submit)
                                             (keyword->pretty-string submit)
                                             submit)
                             collect <input type= "submit" name= "submit" value=?value/>
                             collect " "))))
                (if expand-focus-logic
                    (<script language= "JavaScript">
                             (let ((focus-field
                                    (if offending-fields
                                        (symbol-name (first offending-fields))
                                        (if field-and-initial-value-pairs
                                            (let ((possibly-found (find-if #'form-field-in-table-p field-and-initial-value-pairs :key #'car)))
                                              (if possibly-found
                                                  (symbol-name (form-field-name (first possibly-found)))))
                                            nil))))
                               (when focus-field
                                 (format nil "document.forms.~A.~A.focus();"
                                         (protect-for-dom form-name)
                                         (protect-for-dom focus-field))))))) 
       (form-generation-error (c)
         (<span class= "form-error">
                (format nil "The form could not be created because: ~A"
                        (form-generation-error-reason c)))))))

(defun form-fully-realized-or-die (form situation &key (expected-field-names (active-form-field-names form situation)))
  (let ((found (mapcar #'form-field-name (active-form-fields form situation))))
    (unless (null (set-difference expected-field-names found))
      (error "The field~{~#[~;~:;s~]~0^~}~:*~{~#[ bawwk~; ~A~; ~A and ~A~:;~@{~#[~; and~] ~A~^,~}~]~} ~:*~{~#[~;was~:;were~]~0^~} defined to be a part of the form ~A at compile time, but ~:*~:*~{~#[~;was~:;were~]~0^~} never added to the form with a define-form form." (set-difference expected-field-names found) (form-name form)))))

(defun form-cookie-consistent-or-die (form expected-cookie)
  (when (not (eq (form-cookie form) expected-cookie))
    (error "Cookie of form used in compilation not equal to cookie of form found at runtime.")))

(defun active-form-submit-text (form situation)
  (cdr (find-if (lambda (sit)
                  (situation-intersect situation sit))
                (form-submit-text-mappings form) :key #'car)))

(defmacro form-html ((form-name processing-url &key (submit-text :submit) (method "post") (table-class "webutils-form") error-text offending-fields gate-fields)
                     &body initial-values)
  (let* ((situation (if (listp form-name) (cdr form-name) nil))
         (form-name (if (listp form-name) (car form-name) form-name))
         (form (find-form form-name)))
    (assert form () "Form named ~A not found!" form-name)
    (let ((convert (gensym))
          (field-name (gensym))
          (form-var (gensym))
          (class (gensym))
          (gate-object (gensym)))
      (loop for field in (active-form-field-names form situation)
         do (pushnew
             `(,field
               (or ,(if gate-fields
                        `(if (class-has-slot-p ',field ,class)
                             (funcall (slot-to-accessor ',field ,class) ,gate-object)))
                   (form-field-default-value (form-field ,form-name ,field ,@situation)))) initial-values :key #'car))
      `(let* ((,form-var (find-form ',form-name))
              ,@(if gate-fields
                    `((,gate-object ,gate-fields)
                      (,class (class-name-of-instance ,gate-object)))))
         (flet ((,convert (,field-name)
                  (case ,field-name
                    ,@(remove-duplicates initial-values :key #'car :from-end t)
                    (t nil))))
           (form-cookie-consistent-or-die ,form-var ',(form-cookie form))
           (form-fully-realized-or-die ,form-var ',situation :expected-field-names ',(active-form-field-names form situation))
           (generate-form ,(symbol-name (form-name form))
                          ,processing-url (or (active-form-submit-text ,form-var ',situation)
                                              ,submit-text) ,method ,table-class
                          (loop for field in (active-form-fields-in-correct-order ,form-var ',situation)
                             collect (list field (,convert (form-field-name field))))
                          ,error-text ,offending-fields :expand-focus-logic (multiple-form-block-expand-focus-logic)))))))

(defmacro form-get-url ((form-name url &key submit-text) &body initial-values)
  (let* ((situation (if (listp form-name) (cdr form-name) nil))
         (form-name (if (listp form-name) (car form-name) form-name))
         (form (find-form form-name)))
    (when (set-difference (mapcar #'first initial-values)
                          (active-form-field-names form situation))
      (error "Not every field of the form ~A was found in the values to form-get-url." form-name))
    `(urlstring
      (merge-url ,url
                 (format nil ,(format nil "?~{~A=~~A&~}submit=~~A"
                                      (mapcar #'protect-for-dom (mapcar #'symbol-name (mapcar #'first initial-values))))
                         ,@(mapcar (lambda (thing)
                                     `(funcall (form-field-value-to-string-translator (form-field ,form-name ,(first thing) ,@situation))
                                               ,(second thing))) initial-values)
                         (or ,submit-text
                             (active-form-submit-text (find-form ',form-name) ',situation)
                             "Submit"))))))

(defmacro resubmit-form (processing-url &key (submit-text :submit) (method "post") (table-class "webutils-form") (form-name 'none) (focus-logic t))
  (declare (ignore processing-url submit-text method table-class form-name focus-logic))
  (error "resubmit-form used outside with-processed-form!"))

(defvar *form-param-access-function*)
(defparameter *form-value-macro* (gensym))

(defmacro set-slots-from-form-values (object &environment env)
  (let ((slots (macroexpand-1 *form-value-macro* env))
        (object-name (gensym))
        (class (gensym)))
    `(let* ((,object-name ,object)
            (,class (class-name-of-instance ,object-name)))
       ,@(loop for slot in slots
            collect `(when (class-has-slot-p ',slot ,class)
                       (funcall (fdefinition (list 'setf (slot-to-accessor ',slot ,class))) ,slot ,object-name)))
       ,object-name)))

(defmacro with-processed-form (form-name request &key error success (method :post))
  (let* ((error-tag (gensym))
         (success-tag (gensym))
         (runtime-set (gensym))
         (runtime-get (gensym))
         (situation (if (listp form-name) (cdr form-name) nil))
         (form-name (if (listp form-name) (car form-name) form-name))
         (form (find-form form-name))
         (message (gensym))
         (block-name (gensym))
         (param (gensym))
         (offending-fields (gensym))
         (form-var (gensym))
         (active-fields (active-form-field-names form situation)))
    (flet ((generate-resubmit-form-flet (body)
             `(macrolet ((resubmit-form (processing-url &key (submit-text :submit) (method "post") (table-class "webutils-form") (form ',form-name) (focus-logic t))
                           (let ((form-name (if (listp form) (car form) form)))
                             `(macrolet
                                  ,(unless focus-logic
                                           '((multiple-form-block-expand-focus-logic () nil)))
                                (form-html (,(if (listp form)
                                                 form
                                                 `(,form ,@',situation)) ,processing-url :submit-text ,submit-text :method ,method :table-class ,table-class :error-text ,',message :offending-fields ,',offending-fields)
                                  ,@(loop
                                       with form = (find-form form-name)
                                       for field-name in (active-form-field-names form ',situation)
                                       collect `(,field-name
                                                 (protected-string (,',param ,(symbol-name field-name))))))))))
                ,body)))
     (assert form () "Form named ~A not found!" form-name)
     (assert (find method '(:post :get)))
     `(let (,@active-fields
            ,message
              ,offending-fields
              (,form-var (find-form ',form-name)))
        (symbol-macrolet ((,*form-value-macro* (,@active-fields)))
          (form-cookie-consistent-or-die ,form-var ',(form-cookie form))
          (form-fully-realized-or-die ,form-var ',situation :expected-field-names ',(active-form-field-names form situation))
          (block ,block-name
            (flet ((,runtime-set (field-name value)
                     (ecase field-name
                       ,@(loop for form-field-name in (active-form-field-names form situation)
                            collect (list form-field-name
                                          `(setf ,form-field-name
                                                 value)))))
                   (,runtime-get (field-name)
                     (ecase field-name
                       ,@(loop for form-field-name in (active-form-field-names form situation)
                              collect (list form-field-name form-field-name))))
                   (,param (name)
                     ,(ecase method
                             (:post `(or (body-param (protect-for-dom name) (request-body ,request)) ""))
                             (:get `(or (first (url-query-param (request-url ,request) (protect-for-dom name))) "")))))
              (macrolet ((get-error-message ()
                           ',message))
                (let ((*form-param-access-function* #',param))
                  (tagbody
                     (loop for field in (active-form-fields-in-correct-order ,form-var ',situation)
                        do (let ((value (,param (symbol-name (form-field-name field))))
                                 parsed-value)
                             (multiple-value-bind (accepted message)
                                 (catch *failed-acceptor-form-catch-tag*
                                   (progn
                                     (funcall (form-field-string-acceptor field) value)
                                     (funcall (form-field-length-constraint field) (length value) (form-field-pretty-name field))
                                     (setf parsed-value (funcall (form-field-string-to-value-translator field) value))
                                     t))
                               (if accepted
                                   (,runtime-set (form-field-name field)
                                                 parsed-value)
                                   (progn
                                     (setf ,message message)
                                     (setf ,offending-fields (list (form-field-name field)))
                                     (go ,error-tag))))))
                     (loop for check in (active-form-semantic-checks/variables ,form-var ',situation)
                        do (multiple-value-bind (success reason offending-fields)
                               (apply (cddr check) (mapcar #',runtime-get (car check)))
                             (when (not success)
                               (setf ,message reason)
                               (setf ,offending-fields offending-fields)
                               (go ,error-tag))))
                     (go ,success-tag)
                     ,error-tag
                     ,(generate-resubmit-form-flet `(return-from ,block-name ,error))
                     ,success-tag)
                  ,(generate-resubmit-form-flet success))))))))))

(defun nonempty-string-validator (string)
  (if (eql (length string) 0)
      (fail-check "Please supply a value.")))

(defmacro submit-ecase (request &body cases)
  (let ((submit (gensym)))
    (flet ((%case-1 (thing)
             (if (keywordp thing)
                 `(equal ,submit ',(keyword->pretty-string thing))
                 `(equal ,submit ',thing))))
     `(let ((,submit (or (body-param "submit" (request-body ,request)) "")))
        (cond
          ,@(loop for case in cases
                 for things = (car case)
               collect `((or ,@(if (listp things)
                                   (loop for thing in things
                                      collect (%case-1 thing))
                                   (list (%case-1 things))))
                         ,@(cdr case)))
          (t (signal 'http-internal-server-error)))))))