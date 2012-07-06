(in-package :webutils.xml-mixed-mode)

;;; Using S-XML, parse XML to XML mixed mode's objects

(defun parse-xml-from-file (file)
  (with-open-file (stream file :direction :input)
    (parse-xml-from-stream stream)))

(defun extend-enclosed (enclosed object)
  (make-xml-tag-enclosed
   (let ((tag (etypecase enclosed
                (xml-tag-enclosed (slot-value enclosed 'tag))
                (xml-tag enclosed))))
     (make-xml-tag (slot-value tag 'name) t (slot-value tag 'attributes)))
   (nconc (etypecase enclosed
            (xml-tag-enclosed (slot-value enclosed 'object))
            (xml-tag nil))
          (list object))))

(defun parse-xml-from-stream (stream)
  (let ((s-xml:*ignore-namespaces* t))
    (s-xml:start-parse-xml
     stream
     (make-instance 's-xml:xml-parser-state
                    :seed nil
                    :new-element-hook
                    (lambda (name attributes seed)
                      (declare (ignore seed))
                      (let ((tag (make-xml-tag (symbol-name name)
                                               nil
                                               (alist->plist
                                                (mapcar (lambda (pair)
                                                          (cons
                                                           (symbol-name (car pair))
                                                           (cdr pair)))
                                                        attributes)))))
                        tag))
                    :finish-element-hook
                    (lambda (name attributes parent-seed seed)
                      (declare (ignore name attributes))
                      (if parent-seed
                          (extend-enclosed parent-seed seed)
                          seed))
                    :text-hook
                    (lambda (string seed)
                      (extend-enclosed seed string))))))