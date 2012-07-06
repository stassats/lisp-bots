(defpackage :webutils.misc (:use :cl :araneida :webutils.application)
            (:export :with-body-params :define-configuration-variable :reload-configuration-variables
                     :with-query-params
                     :expire-authorization-tokens :make-authorization-token :is-authorized
                     :expire-request-authorization :handler-url :define-handler-hierarchy))
(in-package :webutils.misc)
(webutils::export-all :webutils.misc)

(defun conanicalize-body-param (param)
  (when param
    (map 'string
         #'(lambda (char)
             (if (typep char 'base-char)
                 char
                 #\?)) param)))

(defmacro with-body-params (param-pairs request &body body)
  (when (null body)
    (error "I think you forgot the request argument to with-body-params."))
  (let ((request-name (gensym)))
    `(let* ((,request-name ,request)
            ,@(mapcar #'(lambda (param-pair)
                          (let ((bind-to (if (listp param-pair)
                                             (first param-pair)
                                             param-pair))
                                (param-name (if (listp param-pair)
                                                (second param-pair)
                                                (string-downcase (symbol-name param-pair)))))
                            `(,bind-to
                              (conanicalize-body-param
                               (body-param ,param-name
                                           (request-body ,request-name))))))
                      param-pairs))
       ,@body)))

(defmacro with-query-params (param-pairs request &body body)
  (when (null body)
    (error "I think you forgot the request argument to with-query-params."))
  (let ((url-name (gensym)))
    `(let* ((,url-name (request-url ,request))
            ,@(mapcar #'(lambda (param-pair)
                          (let ((bind-to (if (listp param-pair)
                                             (first param-pair)
                                             param-pair))
                                (param-name (if (listp param-pair)
                                                (second param-pair)
                                                (string-downcase (symbol-name param-pair)))))
                            `(,bind-to
                              (first (url-query-param ,url-name ,param-name)))))
                      param-pairs))
       ,@body)))

(defparameter *configuration-variable-reload-hooks* nil)

(defmacro define-configuration-variable (variable value-load-form)
  `(progn
    (defparameter ,variable ,value-load-form)
    (setf (cdr (or (assoc ',variable *configuration-variable-reload-hooks*)
                   (first (push (cons ',variable nil) *configuration-variable-reload-hooks*))))
          (lambda ()
            (declare (special ,variable))
            (setf ,variable ,value-load-form)))))

(defun reload-configuration-variables ()
  (loop for hook in (reverse *configuration-variable-reload-hooks*)
       do (funcall (cdr hook))))

(defparameter *authorization-tokens* nil)

(defun expire-authorization-tokens (&key (timeout (* 60 60)) (filter (constantly nil)))
  (let ((now (get-universal-time)))
    (setf *authorization-tokens*
          (remove-if #'(lambda (token)
                         (or (< (+ (first token) timeout) now)
                             (funcall filter (third token))))
                     *authorization-tokens*))))

(defun expire-request-authorization (request)
  (let ((token (is-authorized request)))
    (if token
        (setf *authorization-tokens*
              (remove token *authorization-tokens* :key #'second)))))

(defun make-authorization-token (&key extra (path "/"))
  (let ((token (random 10000000)))
    (push (list (get-universal-time) token extra)
          *authorization-tokens*)
    (format nil "AUTHTOKEN=~A; path=~A" token path)))

(defun is-authorized (request &key (update t) (extra nil extra-supplied-p) (test 'eql))
  (let* ((token (parse-integer (or (request-cookie request "AUTHTOKEN") "-1") :junk-allowed t))
         (found (find token *authorization-tokens* :key #'second)))
    (when found
      (when update (setf (first found) (get-universal-time)))
      (if extra-supplied-p
         (values (funcall test extra (third found)) token)
         (values token (third found))))))

(defgeneric handler-url (handler-class-name))

(defmacro define-handler-hierarchy (listener-or-application &rest hierarchy)
  (let (handler-class-definitions)
    (multiple-value-bind (listener extra-superclasses)
        (if (listp listener-or-application)
            (ecase (first listener-or-application)
              (:listener (values (second listener-or-application) nil))
              (:application (values `(application-listener (find-application ',(second listener-or-application)))
                                    (list (second listener-or-application))))))
      ;; a four of three elements - class name, superclasses, URL root, and inexact-p
      (labels ((parse-root-group (root-group existing-root)
                 (let ((new-url (if existing-root
                                    `(merge-url ,existing-root ,(first root-group))
                                    (first root-group))))
                   (if (and (or (eql (length root-group) 2)
                                (eql (length root-group) 4))
                            (or (symbolp (second root-group))
                                (every #'symbolp (second root-group)))
                            (or (eql (length root-group) 2)
                                (and (eq (third root-group) :inexact))))
                       ;; it's actually a single handler definition
                       (push (list (if (symbolp (second root-group))
                                       (second root-group)
                                       (first (second root-group)))
                                   (if (symbolp (second root-group))
                                       nil
                                       (cdr (second root-group)))
                                   new-url
                                   (if (eq (length root-group) 4)
                                       (fourth root-group)))
                             handler-class-definitions)
                       (loop for sub-root-group in (cdr root-group)
                          do (parse-root-group sub-root-group new-url))))))
        (loop for group in hierarchy
           do (parse-root-group group nil)))
      `(progn
         ,@(loop for (class-name superclasses root-url inexact) in handler-class-definitions
              with g = (gensym)
              collect `(defclass ,class-name (handler ,@superclasses ,@extra-superclasses) ())
              collect `(defmethod handler-url ((,g (eql ',class-name)))
                         ,root-url)
              collect `(install-handler (http-listener-handler ,listener)
                                        (make-instance ',class-name)
                                        (urlstring (handler-url ',class-name))
                                        (not ,inexact)))))))