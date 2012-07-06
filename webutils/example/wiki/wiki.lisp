(defpackage :webutils-wiki-example (:use :cl :webutils :araneida :html-encode))
(in-package :webutils-wiki-example)

(defvar *wiki-root* (load-time-value (make-pathname :directory (pathname-directory *load-pathname*))))

(define-configuration-variable *wiki-url* (parse-urlstring "http://localhost:8080/"))

(define-configuration-variable *wiki-template*
    (load-template (merge-pathnames "wiki.template" *wiki-root*) :tags-package #.*package*))

(defvar *listener*
  (make-instance 'serve-event-http-listener :port 8080))

(defvar *wiki-pages* nil)

(define-simple-serialized-class wiki-page ()
  ((name :initarg :name :accessor wiki-page-name)
   (summary :initarg :summary :accessor wiki-page-summary)
   (contents :initarg :contents :accessor wiki-page-contents)
   (parent :initarg :parent :accessor wiki-page-parent :initform nil)
   (children :initarg :children :accessor wiki-page-children :initform nil))
  (:key-slot name)
  (:store *wiki-pages*)
  (:store-directory (merge-pathnames (make-pathname :directory '(:relative "wiki-pages"))
                                     *wiki-root*)))

(define-object-relations wiki-page
    (wiki-page-parent wiki-page-children :type :one-to-many))

(defclass wiki-handler (handler) ())

(defclass view-wiki-handler (wiki-handler) ())

(define-configuration-variable *view-wiki-url*
    (merge-url *wiki-url* "view/"))

(defclass needs-captcha-mixin () ())

(defclass edit-wiki-handler (wiki-handler needs-captcha-mixin) ())

(define-configuration-variable *edit-wiki-url*
    (merge-url *wiki-url* "edit"))

(defclass new-wiki-handler (wiki-handler needs-captcha-mixin) ())

(define-configuration-variable *new-wiki-url*
    (merge-url *wiki-url* "new"))

(defclass captcha-handler (wiki-handler) ())

(define-configuration-variable *captcha-url*
    (merge-url *wiki-url* "captcha"))

(defmethod handle-request-response :before ((handler wiki-handler) method request)
  (expire-captchas)
  (expire-authorization-tokens))

(define-form wiki-form ()
    (name summary contents parent)
  (:semantic-check
   :create (if (find-instance-by-key name 'wiki-page)
               (fail-check "A wiki page with this name already exists." name))))

(define-form-field (wiki-form name :create) form-field
  :string-acceptor (lambda (string)
                     (if (not (every #'alphanumericp string))
                         (fail-check "Wiki page names must be alphanumeric only."))))

(define-form-field (wiki-form name :edit) hidden-form-field
  :string-acceptor (lambda (string)
                     (if (not (find-instance-by-key string 'wiki-page))
                         (fail-check "What did you do to the form name?"))))

(define-form-field (wiki-form summary) form-field
  :string-acceptor (lambda (string)
                     (if (not (> (length string) 0))
                         (fail-check "Please provide a wiki page summary."))))

(define-form-field (wiki-form contents) textarea-form-field
  :string-acceptor #'nonempty-string-validator)

(define-form-field (wiki-form parent :create) hidden-form-field
  :string-acceptor (lambda (string)
                     (if (not (find-instance-by-key string 'wiki-page))
                         (fail-check "Can't find supposed parent page.")))
  :string-to-value-translator (lambda (string)
                                (find-instance-by-key string 'wiki-page))
  :value-to-string-translator (lambda (value)
                                (wiki-page-name value)))

(define-form captcha () (question answer continue-url id)
  (:semantic-check
   (unless (check-captcha id answer)
     (fail-check "You failed to answer the question correctly."))))

(define-form-field (captcha question) immutable-form-field)

(define-form-field (captcha answer) form-field
  :string-acceptor (lambda (string)
                     (if (not (and (> (length string) 0)
                                   (every #'digit-char-p string)))
                         (fail-check "Please enter only numbers for the answer."))))

(define-form-field (captcha continue-url) hidden-form-field
  :string-acceptor #'nonempty-string-validator)

(define-form-field (captcha id) hidden-form-field
  :string-acceptor (lambda (string)
                     (if (not (and (> (length string) 0)
                                   (every #'digit-char-p string)))
                         (fail-check "What did you do to the id?")))
  :string-to-value-translator #'parse-integer
  :value-to-string-translator #'prin1-to-string)

(define-form wiki-action () (page))

(define-form-field (wiki-action page) hidden-form-field
  :string-acceptor (lambda (string)
                     (if (not (find-instance-by-key string 'wiki-page))
                         (fail-check "Huh?")))
  :value-to-string-translator #'wiki-page-name
  :string-to-value-translator (lambda (string)
                                (find-instance-by-key string 'wiki-page)))

(defvar *current-wiki-page* nil)

(defun wiki-wrap-page (request title page)
  (let-tags
      ((page-title (stream)
                   (write-string title stream))
       (page-controls
        (stream)
        (when *current-wiki-page*
          (html-stream stream
                       `(div
                         ,@(loop for page = *current-wiki-page*
                              then (wiki-page-parent page)
                              while page
                              collect
                                `((a :href ,(urlstring (merge-url *view-wiki-url*
                                                                  (wiki-page-name page))))
                                  ,(wiki-page-name page))
                              if (wiki-page-parent page)
                              collect " &gt; ")
                         (br)
                         ,(form-html (wiki-action *edit-wiki-url* :submit-text "Edit" :method "get")
                                     (page *current-wiki-page*))
                         ,(form-html (wiki-action *new-wiki-url* :submit-text "New Page Here" :method "get")
                                     (page *current-wiki-page*))))))
       (page-content
        (stream)
        (loop for thing in page do (html-stream stream thing))))
    (expand-template-body *wiki-template* (request-stream request))
    t))

(defmethod handle-request-response ((handler view-wiki-handler) method request)
  (request-send-headers request :expires 0)
  (let* ((wiki-page-name (request-unhandled-part request))
         (wiki-page-name (if (eql (length wiki-page-name) 0)
                             "WebHome" wiki-page-name))
         (wiki-page (find-instance-by-key wiki-page-name 'wiki-page)))
    (if wiki-page
        (let ((*current-wiki-page* wiki-page))
          (wiki-wrap-page
           request
           (wiki-page-name wiki-page)
           `((i ,(encode-for-pre (wiki-page-summary wiki-page)))
             (p)
             ,(encode-for-pre (wiki-page-contents wiki-page))
              (p)
              (ul
               ,@(loop for child in (wiki-page-children wiki-page)
                    collect `(li ((a :href ,(urlstring (merge-url *view-wiki-url* (wiki-page-name child))))
                                  ,(wiki-page-name child))))))))
        (wiki-wrap-page
         request
         "Not Found"
         `("I couldn't find a wiki page with that name.")))))

(defmethod handle-request-response :around ((handler needs-captcha-mixin) method request)
  (if (is-authorized request)
      (call-next-method)
      (progn
        (request-send-headers request :expires 0)
        (wiki-wrap-page
         request
         "Please answer the captcha question"
         (multiple-value-bind (id question)
             (generate-captcha)
           `(,(form-html (captcha *captcha-url*)
                         (question question)
                         (id id)
                         (continue-url (urlstring (request-url request))))))))))

(defmethod handle-request-response ((handler captcha-handler) (method (eql :get)) request)
  nil)

(defmethod handle-request-response ((handler captcha-handler) (method (eql :post)) request)
  (with-processed-form
      captcha request
      :error (progn
               (request-send-headers request :expires 0)
               (wiki-wrap-page
                request
                "Oops."
                `(,(resubmit-form (request-url request)))))
      :success
      (progn
        (request-send-headers request :expires 0 :set-cookie (make-authorization-token))
        (wiki-wrap-page
         request
         "Correct!"
         `("You obviously took a math class."
           (p)
           ((a :href ,continue-url)
            "Go to what you were trying to do."))))))

(defmethod handle-request-response ((handler new-wiki-handler) (method (eql :get)) request)
  (request-send-headers request :expires 0)
  (wiki-wrap-page
   request
   "New Wiki Page"
   `(,(form-html (wiki-form (request-url request) :situation :create)))))

(install-handler (http-listener-handler *listener*)
                 (make-instance 'view-wiki-handler)
                 (urlstring *view-wiki-url*) nil)

(install-handler (http-listener-handler *listener*)
                 (make-instance 'captcha-handler)
                 (urlstring *captcha-url*) t)

(install-handler (http-listener-handler *listener*)
                 (make-instance 'new-wiki-handler)
                 (urlstring *new-wiki-url*) nil)

(defun ensure-web-home ()
  (if (not (find-instance-by-key "WebHome" 'wiki-page))
      (register-instance (make-instance 'wiki-page
                                        :name "WebHome"
                                        :summary "The home page for this wiki."
                                        :contents "This is an empty wiki."))))

(defun start-wiki ()
  (start-listening *listener*)
  (load-store-for-class 'wiki-page))