(defpackage :webutils (:use :cl))
(in-package :webutils)

(defun export-all (&rest packages)
  (loop for package in packages
       do (loop for symbol being each external-symbol of package
               do (progn
                    (import symbol :webutils)
                    (export symbol :webutils)))))

