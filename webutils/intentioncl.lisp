(defpackage :webutils.intentioncl (:use :cl))
(in-package :webutils.intentioncl)
;(webutils::export-all :webutils.intentioncl)

(defun mangle-name-for-class-id (string)
  (with-output-to-string (output-stream)
    (with-input-from-string (input-stream string)
      (loop for char = (read-char input-stream nil nil)
           while char
           if (char= char #\:)
           do (write-string "\\:" output-stream)
           else if (char= char #\\)
           do (write-string "\\\\" output-stream)
         else do (write-char char output-stream)))))

(defun unmangle-name-for-class-id (string)
  (with-output-to-string (output-stream)
    (with-input-from-string (input-stream string)
      (loop for char = (read-char input-stream nil nil)
           while char
           if (char= char #\\)
           do (write-char (read-char input-stream) output-stream)
           else do (write-char char output-stream)))))

(defun split-name-at-colon (name)
  (let ((position (position-if (let (skip)
                                 (lambda (char)
                                   (cond
                                     (skip (setf skip nil))
                                     ((char= char #\\)
                                      (progn (setf skip t) nil))
                                     (t (char= char #\:)))))
                               name)))
    (assert position (name) "Colon not found in name ~A" name)
    (list (subseq name 0 position) (subseq name (1+ position)))))

(defun make-mangled-symbol-name (symbol)
  (let ((package (symbol-package symbol))
        (mangled-name (mangle-name-for-class-id (symbol-name symbol))))
    (if package
        (concatenate 'string (mangle-name-for-class-id (package-name package))
                     ":" mangled-name)
        (concatenate 'string ":" mangled-name))))

(defpackage :webutils.intentioncl-types (:use))

(defvar *intentioncl-class-mapping-indicator*
  (make-symbol "INTENTIONCL-CLASS-MAP"))

(defun make-intentional-type-class-name (symbol)
  (let* ((new-name-as-string (make-mangled-symbol-name symbol))
         (new-name (intern new-name-as-string :webutils.intentioncl-types)))
    (when (or
           (not (eql (get symbol *intentioncl-class-mapping-indicator* new-name) new-name))
           (not (eql (get new-name *intentioncl-class-mapping-indicator* symbol) symbol)))
      (error "The symbol ~S has had its home package moved since it was used to designate an intentional type." symbol)) 
    (setf (get new-name *intentioncl-class-mapping-indicator*)
          symbol
          (get symbol *intentioncl-class-mapping-indicator*)
          new-name)
    new-name))

(defstruct (destructuring-lambda-list (:type list))
  wholevar
  reqvars
  optvars
  restvar
  keyvars
  allow-other-keys)

(defun canonicalize-keyvar (keyvar)
  (typecase keyvar
    (symbol `((,(intern (symbol-name keyvar) :keyword) ,keyvar) nil ,(gensym)))
    (list (destructuring-bind (var &optional init-form (supplied-p nil supplied-p-supplied))
              keyvar
            `(,(typecase var
                         (symbol `(,(intern (symbol-name var) :keyword) ,var))
                         (list var))
               ,init-form
               ,(if supplied-p-supplied
                    supplied-p
                    (gensym)))))))

(defun canonicalize-optvar (optvar)
  (typecase optvar
    (symbol `(,optvar nil ,(gensym)))
    (list (destructuring-bind (var &optional init-form (supplied-p nil supplied-p-supplied))
              optvar
            `(,var
              ,init-form
              ,(if supplied-p-supplied
                   supplied-p
                   (gensym)))))))

(defmacro with-consumer ((consumer list) &body body)
  (let ((list-var (gensym)))
    `(let ((,list-var ,list))
       (labels ((,consumer ()
                  (if (null ,list-var)
                      (values nil t)
                      (values (pop ,list-var) nil))))
         ,@body))))

(defmacro push-end (value place)
  `(setf ,place
         (append ,place (list ,value))))

(defun canonicalize-destructuring-lambda-list (lambda-list)
  (if (not (listp lambda-list))
      `(&rest ,lambda-list)
      (labels ((proper-list-p (list)
                 (cond ((null list) t)
                       ((not (listp (cdr list))) nil)
                       (t (proper-list-p (cdr list))))))
        (if (not (proper-list-p lambda-list))
            (append (butlast lambda-list)
                    (list (car (last lambda-list)))
                    (list '&rest (cdr (last lambda-list))))
            lambda-list))))

(defun parse-destructuring-lambda-list (lambda-list)
  (labels ((error-when (condition)
             (when condition
               (error "Malformed destructuring lambda list: ~S" lambda-list))))
    (let ((parsed (make-destructuring-lambda-list))
          (canonicalized (canonicalize-destructuring-lambda-list lambda-list))
          found-wholevar found-reqvars found-optvars found-restvar found-keyvars found-allow-other-keys)
      (when (find '&aux canonicalized)
        (error "&aux not supported in intentional type destructuring lambda list ~S" lambda-list))
      (with-consumer (consumer canonicalized)
        (loop
           (tagbody
              (multiple-value-bind (value done)
                  (consumer)
                (when done (return parsed))
                (when (eq value '&whole)
                  (error-when found-wholevar)
                  (setf found-wholevar t)
                  (setf (destructuring-lambda-list-wholevar parsed)
                        (consumer))
                  (go continue))
                (when (eq value '&optional)
                  (error-when found-optvars)
                  (setf found-reqvars t found-optvars t)
                  (go continue))
                (when (eq value '&rest)
                  (error-when found-restvar)
                  (setf found-reqvars t found-optvars t found-restvar t)
                  (go continue))
                (when (eq value '&key)
                  (error-when found-keyvars)
                  (setf found-reqvars t found-optvars t found-restvar t found-keyvars t)
                  (go continue))
                (when (eq value '&allow-other-keys)
                  (error-when found-allow-other-keys)
                  (error-when (not found-keyvars))
                  (setf found-allow-other-keys t)
                  (setf (destructuring-lambda-list-allow-other-keys parsed) t)
                  (go continue))
                (error-when found-allow-other-keys)
                (when found-keyvars
                  (push-end (canonicalize-keyvar value) (destructuring-lambda-list-keyvars parsed))
                  (go continue))
                (when found-restvar
                  (error-when (destructuring-lambda-list-restvar parsed))
                  (setf (destructuring-lambda-list-restvar parsed) value)
                  (go continue))
                (when found-optvars
                  (push-end (canonicalize-optvar value) (destructuring-lambda-list-optvars parsed))
                  (go continue))
                (push-end value (destructuring-lambda-list-reqvars parsed)))
            continue))))))

(defun destructuring-lambda-lists-congruent-p (lambda-list-1 lambda-list-2)
  (let ((parsed-1 (parse-destructuring-lambda-list lambda-list-1))
        (parsed-2 (parse-destructuring-lambda-list lambda-list-2)))
    (and (eq (destructuring-lambda-list-wholevar parsed-1)
             (destructuring-lambda-list-wholevar parsed-2))
         (eql (length (destructuring-lambda-list-reqvars parsed-1))
              (length (destructuring-lambda-list-reqvars parsed-2)))
         (eql (length (destructuring-lambda-list-optvars parsed-1))
              (length (destructuring-lambda-list-optvars parsed-2)))
         (eq (destructuring-lambda-list-restvar parsed-1)
             (destructuring-lambda-list-restvar parsed-2))
         (eql (length (destructuring-lambda-list-keyvars parsed-1))
              (length (destructuring-lambda-list-keyvars parsed-2)))
         (every (lambda (x y)
                  (eql (first (first x))
                       (first (first y))))
                (destructuring-lambda-list-keyvars parsed-1)
                (destructuring-lambda-list-keyvars parsed-2))
         (eq (destructuring-lambda-list-allow-other-keys parsed-1)
             (destructuring-lambda-list-allow-other-keys parsed-2)))))

(defvar *intentioncl-class-defining-lambda-list-indicator* (make-symbol "DEFINING-LAMBDA-LIST"))

(defclass intentional-type ()
  ((arguments :initarg :arguments :accessor intentional-type-arguments)
   (name :initarg :name :accessor intentional-type-name :allocation :class)))

(defmethod print-object ((object intentional-type) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "~S ~S"
            'intentional
            (list* (intentional-type-name object)
                   (intentional-type-arguments object)))))

(defmacro define-intentional-type (name &rest arguments)
  (let ((name (make-intentional-type-class-name name)))
    `(progn
       (defclass ,name (intentional-type)
         ((name :initarg :name :initform ',name :accessor intentional-type-name :allocation :class)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name *intentioncl-class-defining-lambda-list-indicator*)
               ',arguments))
       ',name)))

(defun intentional (type-specifier)
  (etypecase type-specifier
    (symbol (intentional (list type-specifier)))
    (cons (let ((name (make-intentional-type-class-name (first type-specifier))))
            (make-instance name
             :arguments (rest type-specifier)
             :name (first type-specifier))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun strip-intentionals (lambda-list)
    (loop with found-non-required = nil
       for entry in lambda-list
       if found-non-required collect entry
       else )))

#+nil
(defmacro define-intentional-generic-function (name lambda-list &rest options)
  )