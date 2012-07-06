(in-package :webutils.xml-mixed-mode)

(defclass translator ()
  ((functions :initarg :functions :accessor translator-functions :initform nil)))

(defclass translator-function ()
  ((name :initarg :name :accessor translator-function-name)
   (function :initarg :function :accessor translator-function-function)
   (arguments :initarg :arguments :accessor translator-function-arguments :initform nil)))

(defmacro bind/fail-k (binding-pattern object fail-k &body body)
  (labels ((proper-list-p (thing)
             (etypecase thing
               (null t)
               (list (proper-list-p (cdr thing)))
               (symbol nil)))
           (build-match-let (whole-var binding-pattern)
             (etypecase binding-pattern
               (null nil)
               (list
                (if (null (cdr binding-pattern))
                    `((,(first binding-pattern) (car ,whole-var)))
                    (let ((new-whole (gensym)))
                      `((,(first binding-pattern) (car ,whole-var))
                        (,new-whole (cdr ,whole-var))
                        ,@(build-match-let new-whole (cdr binding-pattern))))))
               (symbol
                `((,binding-pattern ,whole-var))))))
   (let ((expected-length (if (proper-list-p binding-pattern)
                              (length binding-pattern)
                              nil))
         (object-var (gensym))
         (fail-k-var (gensym))
         (block-name (gensym)))
     `(block ,block-name
        (let ((,object-var ,object)
              (,fail-k-var ,fail-k))
          (declare (ignorable ,fail-k-var))
          ,@(when expected-length
                  `((unless (eql (length ,object-var) ,expected-length)
                      (return-from ,block-name (funcall ,fail-k-var)))))
          (let* ,(build-match-let object-var binding-pattern)
            ,@body))))))

(defmacro fail-match-when (condition)
  `(when ,condition (fail-match)))

(defmacro fail-match-unless (condition)
  `(unless ,condition (fail-match)))

(defparameter *valid-arguments* '(:oaoo :stop))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-xml-pattern-arguments (xml-pattern)
    (when (listp xml-pattern)
      (loop with arguments = nil
         for head = xml-pattern then (cdr head)
           if (not (keywordp (car head)))
           do (return (values arguments head))
           else do (push (car head) arguments))))
  
  (defun validate-xml-pattern-arguments (arguments)
    (let ((difference (set-difference arguments *valid-arguments*)))
      (when difference
        (error "The following arguments are not valid arguments to an XML pattern: ~(~S~^, ~)" difference)))))

(defmacro translator-function (xml-pattern &body body)
  (let ((xml-object (gensym))
        (fail-k (gensym))
        (b (gensym))
        (tag (gensym))
        (tag-body (gensym)))
    `(lambda (,xml-object ,fail-k)
       (block ,b
         (flet ((fail-match ()
                  (return-from ,b (funcall ,fail-k))))
           ,(if (xml-constructor-form-p xml-pattern)
                ;; no body matching in pattern
                `(if (typep ,xml-object 'xml-tag)
                     (destructuring-bind ,(xml-pattern-bindings xml-pattern)
                         (parse-xml-tag-from-pattern ,xml-object ',(main-xml-pattern-form xml-pattern) #'fail-match)
                       ,@body)
                     (fail-match))
                `(let ((,tag (etypecase ,xml-object
                               (xml-tag ,xml-object)
                               (xml-tag-enclosed (slot-value ,xml-object 'tag))))
                       (,tag-body (etypecase ,xml-object
                                    (xml-tag nil)
                                    (xml-tag-enclosed (slot-value ,xml-object 'object)))))
                   (destructuring-bind ,(xml-pattern-bindings (car xml-pattern))
                       (parse-xml-tag-from-pattern ,tag ',(main-xml-pattern-form (car xml-pattern)) #'fail-match)
                     (bind/fail-k ,(cdr xml-pattern) ,tag-body #'fail-match
                       ,@body)))))))))

(defmacro define-translator (translator-object name xml-pattern &body body)
  (multiple-value-bind (arguments xml-pattern)
      (parse-xml-pattern-arguments xml-pattern)
    (validate-xml-pattern-arguments arguments)
    (let ((translator-function (gensym)))
      `(let  ((,translator-function
                  (or (find ',name (translator-functions ,translator-object) :key #'translator-function-name)
                      (car (push (make-instance 'translator-function :name ',name)
                                 (translator-functions ,translator-object))))))
         (setf (translator-function-function ,translator-function)
               (translator-function ,xml-pattern ,@body)
               (translator-function-arguments ,translator-function)
               ',arguments)))))

(defmacro extend-translator (translator (xml-pattern &body translator-body) &body body)
  (multiple-value-bind (arguments xml-pattern)
      (parse-xml-pattern-arguments xml-pattern)
    (validate-xml-pattern-arguments arguments)
    `(let ((,translator
            (make-instance 'translator :functions
                           (cons (make-instance 'translator-function
                                                :name (gensym)
                                                :arguments ',arguments
                                                :function (translator-function ,xml-pattern ,@translator-body))
                                 (translator-functions ,translator)))))
       ,@body)))

(defmacro extend-translator* (translator patterns &body body)
  (if (null patterns)
      `(progn ,@body)
      (labels ((extend (patterns)
                 (if patterns
                     `(extend-translator ,translator ,(first patterns)
                        ,(extend (cdr patterns)))
                     `(progn ,@body))))
        (extend (reverse patterns)))))

(defun make-translator () (make-instance 'translator))

(defun apply-translator (translator xml-object)
  (labels ((apply-translator-to-body (xml-object)
             (if (typep xml-object 'xml-tag-enclosed)
                 (make-xml-tag-enclosed
                  (slot-value xml-object 'tag)
                  (loop for thing in (slot-value xml-object 'object)
                     collect (apply-translator translator thing)))
                 xml-object))
           (apply-translator-functions (functions)
             (if (null functions)
                 (apply-translator-to-body xml-object)
                 (let* ((tf (car functions))
                        (arguments (translator-function-arguments tf))
                        (translated
                         (funcall (translator-function-function tf) xml-object
                                  (lambda ()
                                    (return-from apply-translator-functions
                                      (apply-translator-functions (cdr functions)))))))
                   (return-from apply-translator
                     (cond
                       ((find :stop arguments)
                        translated)
                       ((find :oaoo arguments)
                        (apply-translator-to-body translated))
                       (t (apply-translator translator translated))))))))
    (typecase xml-object
      ((or xml-tag xml-tag-enclosed)
       (apply-translator-functions (translator-functions translator)))
      (list
       (loop for object in xml-object
          collect (apply-translator translator object)))
      (t xml-object))))