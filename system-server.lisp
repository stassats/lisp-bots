(in-package :lisppaste)

(defparameter *memoize-colorize-table* (make-hash-table :test #'equal))

(defun all-system-names ()
  (loop for i being each hash-key of asdf::*defined-systems* collect i))

(defun find-component-from-string (string &key root)
  (multiple-value-bind (component-name start-of-rest)
      (split-sequence:split-sequence #\/ string :count 1)
    (let ((new-root (asdf:find-component root (car component-name))))
      (if new-root
          (if (> (length string) start-of-rest)
              (find-component-from-string (subseq string start-of-rest) :root new-root)
              new-root)))))

(defclass main-system-server-handler (handler) ())

(defclass show-component-handler (handler) ())

(defmethod handle-request-response ((handler main-system-server-handler) method request)
  (request-send-headers request :expires 0)
  (xml-output-to-stream
   (request-stream request)
   (lisppaste-wrap-page
    "Select a System"
    (<div class="controls">
          (<ul>
           (loop for i in (all-system-names)
              for system = (asdf:find-system i)
              collect (<li>
                       (<a href=?(urlstring (merge-url *show-component-url*
                                                       i))>
                           i)
                       " - "
                       (or (ignore-errors (asdf:system-description system))
                           (ignore-errors (asdf:system-long-description system))
                           "No Description"))))))))

(defun memoize-colorize-file (component type)
  (let ((ent (list (asdf:component-pathname component)
                   colorize:*version-token*
                   (file-write-date (asdf:component-pathname component)))))
    (multiple-value-bind (val found) (gethash ent *memoize-colorize-table*)
      (if found
          val
          (setf (gethash ent *memoize-colorize-table*)
                (with-output-to-string (s)
                  (colorize:colorize-file-to-stream type
                                                    (asdf:component-pathname component) s :wrap nil :css-background "paste")))))))
  
(defun component-sorter (c1 c2)
  (if (typep c1 'asdf:module)
      (if (typep c2 'asdf:module)
          (string< (asdf:component-name c1) (asdf:component-name c2))
          t)
      (if (typep c2 'asdf:module)
          nil
          (string< (asdf:component-name c1) (asdf:component-name c2)))))

(defun module-div (component url)
  (<div>
   (when (typep component 'asdf:system)
     (<div class="info-text">
           (<span class="small-header">
                  (format nil "About system \"~A\""
                          (asdf:component-name component)))
           <p/>
           (<table>
            (<tr>
             (<td> (<b> "Name"))
             (<td> (asdf:component-name component)))
            (<tr>
             (<td> (<b> "Version"))
             (<td> (or (ignore-errors (asdf:component-version component)) "None")))
            (<tr>
             (<td> (<b> "Author"))
             (<td> (or (ignore-errors (asdf:system-author component)) "None")))
            (<tr>
             (<td> (<b> "License"))
             (<td> (or (ignore-errors (asdf:system-license component)) "None")))
            (<tr>
             (<td> (<b> "Description"))
             (<td> (or (ignore-errors (asdf:system-description component)) "None")))
            (<tr>
             (<td> (<b> "Long Description"))
             (<td> (or (ignore-errors (asdf:system-long-description component)) "None"))))))
   (<div class="controls">
         (<span class="small-header"> "Select a component:")
         (<ul>
          (loop for i in (sort (copy-list (asdf:module-components component)) #'component-sorter)
              for link = (<a href=?(concatenate 'string
                                                  url
                                                  "/"
                                                  (asdf:component-name i))>
                             (asdf:component-name i))
              if (typep i 'asdf:module) collect (<li> (<b> link))
              else collect (<li> link))))))

(defun file-div (component type)
  (<table width="100%" class="paste-area">
          (<tr>
           (<td bgcolor="#F4F4F4">
                (if (eql type :none)
                    (<pre>
                     (with-output-to-string (s)
                       (with-open-file (f (asdf:component-pathname component) :direction :input)
                         (loop for line = (read-line f nil nil)
                            while line
                            do (progn (write-string line s)
                                      (terpri s))))))
                    (<pre class="paste-area">
                     (make-unescaped-string
                      (memoize-colorize-file component type))))))))

(defmethod handle-request-response ((handler show-component-handler) method request)
  (let ((component (find-component-from-string (request-unhandled-part request))))
    (and component
         (progn
           (request-send-headers request :expires 0)
           (xml-output-to-stream
            (request-stream request)
            (lisppaste-wrap-page
             (format nil "Component ~A" (asdf:component-name component))
             (<div>
              (<div class="controls">
                    "You are here: "
                    (<a href=?(urlstring *main-system-server-url*)>
                        "All Systems")
                    (loop for i in (reverse (maplist #'reverse (nreverse (split-sequence:split-sequence #\/ (request-unhandled-part request)))))
                       collect " / "
                       collect (<a href=?(urlstring (merge-url *show-component-url*
                                                               (format nil "~{~A~^/~}"
                                                                       i)))>
                                   (car (last i)))))
              <p/>
              (typecase component
                (asdf:module (module-div component (urlstring (request-url request))))
                (asdf:cl-source-file (file-div component :common-lisp-file))
                (asdf:static-file
                 (file-div component (if (equalp (pathname-type (asdf:component-pathname component)) "lisp")
                                         :common-lisp-file
                                         :none)))
                (t (<div class="paste-area">
                         "I'm afraid I don't quite know what to do with this file."))))))))))

(when *serve-source*
  (install-handler
   (http-listener-handler *paste-listener*)
   (make-instance 'main-system-server-handler)
   (urlstring *main-system-server-url*) t)
  
  (install-handler
   (http-listener-handler *paste-listener*)
   (make-instance 'show-component-handler)
   (urlstring *show-component-url*) nil))
