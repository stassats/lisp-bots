(defpackage
    :webutils.application (:use :cl :araneida :webutils.xml-mixed-mode :webutils.forms)
    (:export :define-application-handler
             :current-application-handler-class
             :current-request-object-name
             :find-application :application-listener
             :define-application :application-process-form
             :start-application :application-page :application-wrap-page
             :make-listener))
(in-package :webutils.application)
(webutils::export-all :webutils.application)

(defun make-listener (&key (hostname "localhost") (port 8080))
  (make-instance 'serve-event-reverse-proxy-listener
                 :translations
                 `((,(format nil "http://~A:~A/" hostname port)
                     (:wild-host "/")))
                 :address #(0 0 0 0)
                 :port port))

(defvar *applications* nil)

(defgeneric application-wrap-page (application-handler request title body &rest extra-headers))
(defgeneric start-application (application))

(defun find-application (application-name)
  (cdr (assoc application-name *applications*)))

(defun (setf find-application) (new-value application-name)
  (setf (cdr (or (assoc application-name *applications*)
                 (car (push (cons application-name nil) *applications*))))
        new-value))

(defclass application ()
  ((listener :initarg :listener :accessor application-listener :allocation :class)))

(defmethod application-wrap-page ((application-handler application) request title body &rest extra-headers)
  (apply #'request-send-headers request :expires 0 extra-headers)
  (xml-output-to-stream (request-stream request)
    (<html> (<head> (<title> title))
            (<body> body))))

(defmethod start-application ((application application)))

(defmacro application-page ((title &optional handler request &rest extra-headers) &body body)
  `(apply #'application-wrap-page ,handler ,request ,title (list ,@body) (list ,@extra-headers)))

(defmacro define-application (application-name listener-object)
  (let ((application-var (gensym)))
   `(progn
      (defclass ,application-name (application) ())
      (let ((,application-var (find-application ',application-name)))
        (setf (find-application ',application-name)
              (make-instance ',application-name :listener (if ,application-var
                                                              (application-listener ,application-var)
                                                              ,listener-object)))))))

(defmacro application-process-form ((form &key method) &body body)
  (declare (ignore form method body))
  (error "application-process-form used outside of an application handler."))

(defmacro define-application-handler (at-least-one-argument &body arguments)
  (let (qualifier (arguments (cons at-least-one-argument arguments)))
    (loop while (keywordp (car arguments))
         do (push (pop arguments) qualifier))
    (setf qualifier (nreverse qualifier))
    (destructuring-bind ((handler-specifier method-qualifier &optional (request-binding (gensym))) &body body) arguments
      (let ((handler (if (listp handler-specifier)
                         (first handler-specifier)
                         (gensym)))
            (handler-class (if (listp handler-specifier)
                               (second handler-specifier)
                               handler-specifier))
            (method (gensym)))
        `(defmethod handle-request-response ,@qualifier
           ((,handler ,handler-class)
            ,(if (keywordp method-qualifier)
                 `(,method (eql ',method-qualifier))
                 (progn
                   (assert (eq method-qualifier t))
                   method))
            ,request-binding)
           (macrolet ((current-application-handler-class () ',handler-class)
                      (current-request-object-name () ',request-binding)
                      (current-application-handler () ',handler)
                      (current-application-method () ',method-qualifier)
                      (application-page ((title &optional (handler ',handler) (request ',request-binding) &rest extra-headers) &body body)
                        `(application-wrap-page ,handler ,request ,title (list ,@body) ,@extra-headers))
                      (application-process-form ((form &key (method ',(if (keywordp method-qualifier) method-qualifier :post)) (request ',request-binding)) &body body)
                        `(with-processed-form ,form ,request
                                              :method ,method
                                              :error (application-page ("Error")
                                                       (resubmit-form (if (eq ',method :get)
                                                                          (,(intern "HANDLER-URL" :webutils.misc) ',',handler-class)
                                                                          (request-url ,request))
                                                                      :method ,method))
                                              :success (progn ,@body))))
             ,@body))))))