;;;; trackback.lisp - standalone trackback ping

(defpackage :webutils.trackback
  (:use :common-lisp :trivial-http :split-sequence :cl-ppcre)
  (:export :ping :autodetect-ping-for-url :ping-xml-rpc))
(in-package :webutils.trackback)
(webutils::export-all :webutils.trackback)

#+sbcl
(defmacro with-host-timeout (time &body body)
  `(sb-ext:with-timeout ,time ,@body))

#-sbcl
(defmacro with-host-timeout (time &body body)
  (declare (ignore time))
  `(progn ,@body))

(defun mini-xml-read (xml-string)
  (let ((cur-xml (list nil))
        (cur-string (copy-seq ""))
        (cur-tag nil)
        (xml-tag-to-close nil)
        (actual-xml-tag nil))
    (loop for c across xml-string
       do
         (if (not cur-tag)
             (if (char= c #\<)
                 (setf cur-tag (copy-seq ""))
                 (setf cur-string (concatenate 'string cur-string (string c))))
             (if (char= c #\>)
                 (if (string-equal cur-tag
                                   (concatenate 'string "/" (car xml-tag-to-close)))
                     (progn
                       (let ((wrap (pop cur-xml)))
                         (setf (car cur-xml)
                               (append (car cur-xml)
                                       (list (cons
                                              (pop actual-xml-tag)
                                              (append
                                               wrap
                                               (if (> (length cur-string) 0)
                                                   (list cur-string))))))))
                       ;;(format t "cur-xml is now ~S~%" cur-xml)
                       (pop xml-tag-to-close)
                       (setf cur-tag nil)
                       (setf cur-string (copy-seq "")))
                     (if (string-equal (car (split-sequence #\space cur-tag))
                                       "?xml")
                         (setf cur-tag nil)
                         (progn
                           (setf (car cur-xml)
                                 (append (car cur-xml)
                                         (if (> (length cur-string) 0)
                                             (list cur-string))))
                           ;;(format t "cur-xml is now ~S~%" cur-xml)
                           (push nil cur-xml)
                           ;;(format t "cur-xml is now ~S~%" cur-xml)
                           (push cur-tag actual-xml-tag)
                           (push (car (split-sequence #\space cur-tag))
                                 xml-tag-to-close)
                           (setf cur-tag nil)
                           (setf cur-string (copy-seq "")))))
                 (setf cur-tag (concatenate 'string cur-tag (string c))))))
    (if (> (length cur-string) 0)
        ;; hopefully everything's closed off at this point
        (setf (car cur-xml)
              (append (car cur-xml)
                      (list cur-string))))
    (car cur-xml)))

(defun ping (trackback-url &key title url excerpt blog-name)
  (assert (every #'stringp (list title url excerpt blog-name)))
  (block ping-return
    (handler-case
        (with-host-timeout 5
          (destructuring-bind (response headers stream)
              (http-post trackback-url "application/x-www-form-urlencoded"
                         (format nil "~{url=~A&title=~A&excerpt=~A&blog_name=~A~}"
                                 (mapcar #'escape-url-query
                                         (list url title excerpt blog-name))))
            (declare (ignore headers))
            (unwind-protect
                 (if (not (eql response 200))
                     (values nil (format nil "HTTP error ~A" response))
                     (let ((xml (copy-seq ""))
                           (parsed-xml nil)
                           (c (read-char stream))
                           (error nil))
                       (loop while c
                          do
                            (progn
                              (setf xml (concatenate 'string xml (string c)))
                              (setf c (read-char stream nil))))
                       (setf parsed-xml (mini-xml-read xml))
                       (mapc #'(lambda (x)
                                 (when (and (consp x) (string-equal (car x) "response"))
                                   (mapc #'(lambda (x)
                                             (if (and (consp x)
                                                      (string-equal (car x) "error"))
                                                 (setf error
                                                       (not
                                                        (some
                                                         #'(lambda (e)
                                                             (and (stringp e)
                                                                  (eql
                                                                   (parse-integer e :junk-allowed t) 0)))
                                                         (cdr x))))
                                                 (if (and (consp x)
                                                          error
                                                          (string-equal (car x) "message"))
                                                     (return-from ping-return
                                                       (values nil (cdr x))))))
                                         (cdr x)))) parsed-xml)
                       (if error
                           (values nil "Unknown error")
                           t)))
              (if stream (close stream)))))
      (serious-condition () (return-from ping nil)))))

(defmacro aif2 (test-form if-form &optional else-form)
  (let ((test-val (gensym)))
    `(multiple-value-bind (,test-val it) ,test-form
       (if ,test-val
           ,if-form
           ,@(if else-form (list else-form))))))

(defun autodetect-ping-for-url (url)
  (let ((best-guess nil))
    (handler-case
        (with-host-timeout 2
          (destructuring-bind (response headers stream) (http-get url)
            (declare (ignore headers))
            (unwind-protect
                 (if (not (eql response 200))
                     (return-from autodetect-ping-for-url nil)
                     (progn
                       (loop for line = (read-line stream nil nil)
                          with in-rdf = nil
                          with found = nil
                          while line
                          do (progn
                               (if (scan "(?i)<rdf:description" line)
                                   (setf in-rdf t))
                               (aif2 (scan-to-strings "(?i)rdf:about=\"(.+)\"" line)
                                     (if (string= (elt it 0) url)
                                         (setf found t)))
                               (aif2 (scan-to-strings "(?i)dc:identifier=\"(.+)\"" line)
                                     (if (string= (elt it 0) url)
                                         (setf found t)))
                               (aif2 (scan-to-strings "(?i)trackback:ping=\"(.+)\"" line)
                                     (if found
                                         (return-from autodetect-ping-for-url (elt it 0))
                                         (setf best-guess (elt it 0))))
                               (when (scan "(?i)</rdf:rdf>" line)
                                 (setf in-rdf nil)
                                 (setf found nil)))
                          finally (return best-guess)))))))
      (serious-condition () (return-from autodetect-ping-for-url best-guess)))))

(defun ping-xml-rpc (ping-url site-name site-url)
  (handler-case
      (with-host-timeout 5
        (destructuring-bind (response headers stream)
            (http-post ping-url "text/xml"
                       (format nil "
<?xml version=\"1.0\"?>
<methodCall>
<methodName>weblogUpdates.ping</methodName>
<params>
<param>
<value>~A</value>
</param>
<param>
<value>~A</value>
</param>
</params>
</methodCall>"
site-name site-url))
          (declare (ignore headers))
          (unwind-protect
               (if (not (eql response 200))
                   (values nil (format nil "HTTP error ~A" response))
                   (let ((xml (copy-seq ""))
                         (parsed-xml nil)
                         (c (read-char stream)))
                     (loop while c
                           do
                           (progn
                             (setf xml (concatenate 'string xml (string c)))
                             (setf c (read-char stream nil))))
                     (setf parsed-xml (mini-xml-read xml))))
            (if stream (close stream)))))
    (serious-condition () (return-from ping-xml-rpc nil))))