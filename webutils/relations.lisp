(defpackage :webutils.relations (:use :cl :webutils.simple-serialized-classes)
            (:export :define-object-relations))
(in-package :webutils.relations)
(webutils::export-all :webutils.relations)

;; Define a relation between objects. one-to-one and one-to-many
;; relations are supported, but many-to-many relations are not.

(defmacro define-object-relations
    (from-class &body relations)
  (let (registered-actions deleted-actions setf-before-actions setf-after-actions)
    (loop for relation in relations
          do
          (destructuring-bind (from-accessor to-accessor &key
                                             (type :one-to-one)
                                             (aggregator 'cons)
                                             (disaggregator 'remove))
              relation
            (push
             `(let ((value (funcall #',from-accessor a)))
               (when value
                 ,(ecase type
                         (:one-to-one
                          `(funcall #'(setf ,to-accessor) a value))
                         (:one-to-many
                          `(funcall #'(setf ,to-accessor)
                            (funcall (load-time-value (function ,aggregator))
                             a (funcall #',to-accessor value))
                            value)))
                 (when (typep value 'webutils.simple-serialized-classes:simple-serialized-class)
                   (serialize-instance value)))) registered-actions)
            (push
             `(let ((value (funcall #',from-accessor a)))
               (when value
                 ,(ecase type
                         (:one-to-one
                          `(funcall #'(setf ,to-accessor) nil value))
                         (:one-to-many
                          `(funcall #'(setf ,to-accessor)
                            (funcall (load-time-value (function ,disaggregator)) a (funcall #',to-accessor value))
                            value)))
                 (when (typep value 'webutils.simple-serialized-classes:simple-serialized-class)
                   (serialize-instance value)))) deleted-actions)
            (push
             `(let ((old-value (funcall #',from-accessor a)))
               (when old-value
                 ,(ecase type
                         (:one-to-one
                          `(funcall #'(setf ,to-accessor) nil old-value))
                         (:one-to-many
                          `(funcall #'(setf ,to-accessor)
                            (funcall (load-time-value (function ,disaggregator)) a (funcall #',to-accessor old-value))
                            old-value)))
                 (when (typep old-value 'webutils.simple-serialized-classes:simple-serialized-class)
                   (serialize-instance old-value))))
             (cdr (or (assoc from-accessor setf-before-actions)
                      (car (push (cons from-accessor nil) setf-before-actions)))))
            (push
             `(when new-value
               ,(ecase type
                       (:one-to-one
                        `(funcall #'(setf ,to-accessor) a new-value))
                       (:one-to-many
                        `(funcall #'(setf ,to-accessor)
                          (funcall (load-time-value (function ,aggregator)) a (funcall #',to-accessor new-value))
                          new-value)))
               (when (typep new-value 'webutils.simple-serialized-classes:simple-serialized-class)
                 (serialize-instance new-value)))
             (cdr (or (assoc from-accessor setf-after-actions)
                      (car (push (cons from-accessor nil) setf-after-actions)))))))
    `(progn
      (defmethod note-registered-instance ((a ,from-class))
        ,@registered-actions)
      (defmethod note-deleted-instance ((a ,from-class))
        ,@deleted-actions)
      ,@ (loop for (from-accessor . actions) in setf-before-actions
          collect
          `(defmethod (setf ,from-accessor) :before (b (a ,from-class))
            ,@actions))
      ,@ (loop for (from-accessor . actions) in setf-after-actions
          collect
          `(defmethod (setf ,from-accessor) :after (new-value (a ,from-class))
            ,@actions)))))