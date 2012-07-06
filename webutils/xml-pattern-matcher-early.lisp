(in-package :webutils.xml-mixed-mode)

(defun xml-constructor-form-bindings (xml-constructor-form)
  (let (bindings seen-append)
    (labels ((parse-attribute-constructor (attribute)
               (ecase (first attribute)
                 (attribute
                  (when (not (stringp (second attribute)))
                    (assert (symbolp (second attribute)) () "Invalid pattern usage of XML attribute name: ~S" (second attribute))
                    (push (second attribute) bindings))
                  (when (not (stringp (third attribute)))
                    (assert (symbolp (third attribute)) () "Invalid pattern usage of XML attribute value: ~S" (third attribute))
                    (push (third attribute) bindings))
                  (when (fourth attribute)
                    (parse-attribute-constructor (fourth attribute))))
                 (attribute-append
                  (assert (symbolp (second attribute)) () "Invalid pattern usage of XML attribute list: ~S" (second attribute))
                  (assert (not seen-append) () "Invalid use of more than one attribute list in XML pattern: ~S" (second attribute))
                  (setf seen-append (second attribute))
                  (when (third attribute)
                    (parse-attribute-constructor (third attribute)))))))
      (when (not (stringp (second xml-constructor-form)))
        (assert (symbolp (second xml-constructor-form)) () "Invalid pattern usage of XML tag name: ~S" (second xml-constructor-form))
        (push (second xml-constructor-form) bindings))
      (when (fourth xml-constructor-form)
        (parse-attribute-constructor (fourth xml-constructor-form))
        (when seen-append
          (push seen-append bindings)))
      (nreverse bindings))))

(defun xml-constructor-form-p (xml-pattern)
  (and (listp xml-pattern)
       (eq (first xml-pattern) 'make-xml-tag)))

(defun main-xml-pattern-form (xml-pattern)
  ;; xml-pattern could be either an XML constructor form or a list
  ;; with an XML constructor form as its head.
  (cond
    ((xml-constructor-form-p xml-pattern)
     ;; no-body XML
     xml-pattern)
    ((and (listp xml-pattern)
          (eq (first xml-pattern) 'lambda))
     ;; XML with body, but the body is ignored
     (assert (and (listp (third xml-pattern))
                  (xml-constructor-form-p (second (third xml-pattern)))) () "~S was expected to be an XML constructor form, but was not." xml-pattern)
     (second (third xml-pattern)))
    ((and (listp xml-pattern)
          (listp (first xml-pattern))
          (eq (first (first xml-pattern)) 'lambda))
     (main-xml-pattern-form (first xml-pattern)))
    (t (error "~S doesn't look like any recognizable sort of XML constructor pattern." xml-pattern))))

(defun xml-pattern-bindings (xml-pattern)
  (xml-constructor-form-bindings (main-xml-pattern-form xml-pattern)))

(defun plist->alist (list)
  (reduce
   (let ((alternate t))
    (lambda (existing thing)
      (if alternate
          (progn
            (setf alternate nil)
            (cons thing existing))
          (progn
            (setf alternate t)
            (cons (cons (car existing) thing) (cdr existing))))))
   list :initial-value nil))

(defun alist->plist (alist)
  (loop for key/value in alist
       collect (car key/value)
       collect (cdr key/value)))

(defun parse-xml-tag-from-pattern (xml-tag xml-constructor-form fail-parse-k)
  (check-type xml-tag xml-tag)
  (let (parse remaining-arguments seen-append)
    (labels ((fail ()
               (return-from parse-xml-tag-from-pattern (funcall fail-parse-k)))
             (parse-attribute-constructor (attribute)
               (ecase (first attribute)
                 (attribute
                  (let ((found (find-if (lambda (attr/value-pair)
                                          (and
                                           (if (stringp (second attribute))
                                               (string= (car attr/value-pair) (second attribute))
                                               t)
                                           (if (stringp (third attribute))
                                               (string= (cdr attr/value-pair) (third attribute))
                                               t)))
                                        remaining-arguments)))
                    (when (not found)
                      (fail))
                    (when (not (stringp (second attribute)))
                      (push (car found) parse))
                    (when (not (stringp (third attribute)))
                      (push (cdr found) parse))
                    (setf remaining-arguments (remove found remaining-arguments))
                    (when (fourth attribute)
                      (parse-attribute-constructor (fourth attribute)))))
                 (attribute-append
                  (setf seen-append (second attribute))
                  (when (third attribute)
                    (parse-attribute-constructor (third attribute)))))))
      (when (stringp (second xml-constructor-form))
        (unless (string= (second xml-constructor-form) (slot-value xml-tag 'name))
          (fail)))
      (when (not (stringp (second xml-constructor-form)))
        (push (slot-value xml-tag 'name) parse))
      (setf remaining-arguments (plist->alist (slot-value xml-tag 'attributes)))
      (when (fourth xml-constructor-form)
        (parse-attribute-constructor (fourth xml-constructor-form))
        (when seen-append
          (push (alist->plist remaining-arguments) parse)
          (setf remaining-arguments nil)))
      (when remaining-arguments (fail))
      (nreverse parse))))