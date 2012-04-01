(defpackage :cocoa-lookup (:use :common-lisp)
            (:export :populate-table :symbol-lookup))
(in-package :cocoa-lookup)

(defparameter *appkit-root* "http://developer.apple.com/documentation/Cocoa/Reference/ApplicationKit/ObjC_classic/")
(defparameter *foundation-root* "http://developer.apple.com/documentation/Cocoa/Reference/Foundation/ObjC_classic/")

(defparameter *appkit-file*
  (merge-pathnames "appkit.lisp-expr"
		   (or #.*compile-file-truename* *default-pathname-defaults*)))

(defparameter *foundation-file*
  (merge-pathnames "foundation.lisp-expr"
		   (or #.*compile-file-truename* *default-pathname-defaults*)))

(defvar *table* nil)

(defvar *populated-p* nil)

(defun populate-table ()
  (unless *populated-p*
    (with-open-file (r *appkit-file* :direction :input)
      (setf *table* (make-hash-table :test #'equalp))
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*)
				  (concatenate 'string *appkit-root* (cdr i))))))
    (with-open-file (r *foundation-file* :direction :input)
      (let ((s (read r)))
        (loop for i in s do (setf (gethash (car i) *table*)
				  (concatenate 'string *foundation-root* (cdr i))))))
    'done)
  (setf *populated-p* t))

(defun symbol-lookup (symbol)
  (unless *populated-p*
    (populate-table))
  (multiple-value-bind (val found)
      (gethash symbol *table*)
    (if found
        val)))
