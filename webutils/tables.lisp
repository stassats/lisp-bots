(defpackage :webutils.tables (:use :cl :webutils.forms :webutils.misc :araneida :webutils.xml-mixed-mode)
            (:export :define-table-generator :define-extended-table-generator :table-generator-form))
(in-package :webutils.tables)
(webutils::export-all :webutils.tables)

(defun sort-fn (thing)
  (ecase thing
    (:descending #'string>)
    (:ascending #'string<)))

(defun extract-sort-by (request column-triads)
  (declare (ignore request))
  (flet ((getp (string)
           (if (boundp 'webutils.forms::*form-param-access-function*)
               (funcall webutils.forms::*form-param-access-function* string)
               nil)))
    (let ((webutils-table-sort-by (getp "WEBUTILS-TABLE-SORT-BY"))
          (webutils-table-sort-then-by (getp "WEBUTILS-TABLE-SORT-THEN-BY"))
          (webutils-table-sort-direction (getp "WEBUTILS-TABLE-SORT-DIRECTION")))
     (if (not (zerop (length webutils-table-sort-by)))
         (list (pretty-string->keyword webutils-table-sort-by)
               (pretty-string->keyword webutils-table-sort-then-by)
               (pretty-string->keyword webutils-table-sort-direction))
         (list (car (first column-triads)) (car (if (cdr column-triads)
                                                    (second column-triads)
                                                    (first column-triads))) :ascending)))))

(defun generate-table (request list column-triads)
  (destructuring-bind (sort-by sort-then-by sort-direction)
      (extract-sort-by request column-triads)
    (let* ((sort-elt 0)
           (sort-then-elt 0)
           (presort-data (loop for elt in list
                            collect (loop for thing in column-triads
                                       for x from 0
                                       if (eq (first thing) sort-by)
                                       do (setf sort-elt x)
                                       if (eq (first thing) sort-then-by)
                                       do (setf sort-then-elt x)
                                       collect  (list (funcall (second thing) elt)
                                                      (princ-to-string (funcall (third thing) elt))))))
           (sorted (stable-sort presort-data
                                #'(lambda (a b)
                                    (let ((sort-1 (second (elt a sort-elt)))
                                          (sort-2 (second (elt b sort-elt))))
                                      (if (string= sort-1 sort-2)
                                          (let ((sort-1 (second (elt a sort-then-elt)))
                                                (sort-2 (second (elt b sort-then-elt))))
                                            (funcall (sort-fn sort-direction) sort-1 sort-2))
                                          (funcall (sort-fn sort-direction) sort-1 sort-2))))))
           (sorted (mapcar #'(lambda (e)
                               (mapcar #'first e)) sorted)))
      (<table class="webutils-table">
              (<tr>
               (loop for thing in column-triads
                  collect (<th> (keyword->pretty-string (first thing)))))
              (loop for elt in sorted
                 for alternate = t then (not alternate)
                 for alternate-class = (if alternate (list "class" "alternate"))
                 collect
                 (<tr $alternate-class>
                      (loop for thing in elt
                         collect (<td> thing))))
              (unless sorted
                (<tr class="alternate">
                 (<th colspan=?(prin1-to-string (length column-triads))>
                      "No table elements.")))))))

(defmacro define-table-generator (function-name &body column-triads)
  `(progn
     (define-form ,function-name () ((webutils-table-sort-by t) (webutils-table-sort-then-by t) (webutils-table-sort-direction t)))
     (define-form-field (,function-name webutils-table-sort-by) selector-form-field
       :pretty-name "Sort By"
       :allowed-values-generator (lambda ()
                                   ',(mapcar #'car column-triads))
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value ',(car (first column-triads)))
     (define-form-field (,function-name webutils-table-sort-by :hidden-resort) hidden-form-field
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value ',(car (first column-triads)))
     (define-form-field (,function-name webutils-table-sort-then-by) selector-form-field
       :pretty-name "Then Sort By"
       :allowed-values-generator (lambda ()
                                   ',(mapcar #'car column-triads))
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value ',(car (if (cdr column-triads)
                                 (second column-triads)
                                 (first column-triads))))
     (define-form-field (,function-name webutils-table-sort-then-by :hidden-resort) hidden-form-field
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value ',(car (if (cdr column-triads)
                                 (second column-triads)
                                 (first column-triads))))
     (define-form-field (,function-name webutils-table-sort-direction) selector-form-field
       :pretty-name "Sort Direction"
       :allowed-values-generator (lambda ()
                                   (list :ascending :descending))
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value :ascending)
     (define-form-field (,function-name webutils-table-sort-direction :hidden-resort) hidden-form-field
       :string-to-value-translator #'pretty-string->keyword
       :value-to-string-translator #'keyword->pretty-string
       :default-value :ascending)
     (defun ,function-name (list request)
       (let ((fields (list ,@(mapcar #'(lambda (e)
                                         (list 'list `',(first e) (second e)
                                               (if (eql (length e) 3)
                                                   (third e)
                                                   (second e)))) column-triads))))
         (generate-table request list fields)))))

(defun generate-extended-table (object groups)
  (let* ((maxspan
          (loop for group in groups
             maximizing (* 2 (length (cdr group)))))
         (maxspan-attribute
          (princ-to-string (max 1 maxspan))))
    (<table class= "webutils-extended-table">
            (loop for group in groups
               for group-name = (first group)
               collect
               (<tr class= "group-header" > (<th colspan=?maxspan-attribute class= "group-header">
                                                 (keyword->pretty-string (first group))))
               collect
               (<tr class=?group-name>
                    (loop for (thing not-last-p) on (cdr group)
                       for count = 1 then (+ count 2)
                       for colspan-attribute = (princ-to-string
                                                (if not-last-p 1
                                                    (max 1 (- maxspan count))))
                       collect
                       (<th> (keyword->pretty-string (first thing)))
                       collect
                       (<td colspan=?colspan-attribute>
                            (funcall (second thing) object))))))))

(defmacro define-extended-table-generator (function-name &body groups)
  `(defun ,function-name (object)
     (generate-extended-table object
                              (list ,@(loop for group in groups
                                           collect
                                           (list* 'list `',(first group)
                                                  (loop for thing in (cdr group)
                                                       collect (list 'list `',(first thing)
                                                                     (second thing)))))))))