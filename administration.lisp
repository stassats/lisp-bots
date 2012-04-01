;;;; $Id: administration.lisp,v 1.6 2009-05-26 23:29:45 lisppaste Exp $
;;;; $Source: /project/lisppaste/cvsroot/lisppaste2/administration.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :lisppaste)

(define-template-form-field password-field password-form-field
  :string-to-value-translator (lambda (string)
				(if (zerop (length string))
				    nil
				    (hash-password-to-vector string)))
  :value-to-string-translator (constantly ""))

(define-template-form-field integer-field form-field
  :string-acceptor (lambda (string)
		     (unless (and (> (length string) 0)
				  (every #'digit-char-p string))
		       (fail-check "Please enter an integer.")))
  :string-to-value-translator #'parse-integer
  :value-to-string-translator #'prin1-to-string)

(define-form login ()
    (password)
  (:semantic-check
   (unless (find password *hashed-administrator-passwords* :test #'equalp)
     (fail-check "Please enter a valid administrator password." password))))

(define-form-field (login password) password-field)

(define-form kill-paste ()
  (paste-number)
  (:semantic-check
   (unless (find-paste paste-number)
     (fail-check "Please enter a valid paste number." paste-number))))

(define-form-field (kill-paste paste-number) integer-field)

(define-form spam-review-action ()
    (action paste))

(define-form-field (spam-review-action action) selector-form-field
  :allowed-values-generator (constantly '(:keep-paste :kill-paste))
  :string-to-value-translator #'pretty-string->keyword
  :value-to-string-translator #'keyword->pretty-string)
(define-form-field (spam-review-action paste) hidden-paste-field)

(define-application-handler
    :around (admin-mixin t request)
    (expire-authorization-tokens)
    (if (is-authorized request :extra :administrator)
	(call-next-method)
	(application-page ("Oops.")
	  (form-html (login (handler-url 'login-handler))))))

(defun redirect-to (request location &rest extra-headers)
  (apply #'request-send-headers
	 request
	 :location location
	 :expires "Fri, 30 Oct 1998 14:19:41 GMT"
	 :pragma "no-cache"
	 :response-code 302 :response-text "Redirected"
	 :content-type "text/plain"
	 extra-headers)
  (write-string "Redirected!" (request-stream request)))

(define-application-handler ((handler login-handler) :post request)
  (application-process-form (login)
     (redirect-to request (urlstring (handler-url 'administration-handler))
		  :set-cookie (make-authorization-token :extra :administrator))))

(define-application-handler ((handler logout-handler) :get request)
  (expire-request-authorization request)
  (redirect-to request *paste-external-url*))

(define-application-handler ((handler administration-handler) :get request)
  (application-page ("Administration menu")
    (<h2> "Spam Review")
    (if (find-spammy-paste)
	(<a href=? (urlstring (handler-url 'spam-review-handler))>
	    (format nil "Spam Review has ~A new spams and pastes requested for deletion." (count-spammy-pastes)))
	"No spam today.")
    (<h2> "Kill a paste")
    (form-html (kill-paste (handler-url 'kill-paste-handler)))
    (<h2> "All done?")
    (<a href=? (urlstring (handler-url 'logout-handler))>
	"Logout.")))

(define-application-handler ((handler kill-paste-handler) :post request)
  (application-process-form (kill-paste)
    (kill-paste paste-number)
    (application-page ("Kill Paste")
      "The paste "
      (<a href=? (urlstring (merge-url *display-paste-url* (prin1-to-string paste-number))) >
	  (prin1-to-string paste-number))
      " has successfully been deleted."
      <p/>
      (<a href=? (urlstring (handler-url 'administration-handler)) >
	  "Back to the administration menu."))))

(defun find-spammy-paste ()
  (or (car (list-pastes :maybe-spam t :limit 1))
      (car (list-pastes :deletion-requested t :limit 1))))

(defun count-spammy-pastes ()
  (+ (count-pastes :maybe-spam t)
     (count-pastes :deletion-requested t)))

(defun spam-review ()
  (let ((paste (find-spammy-paste)))
    (cond
      ((not paste)
       (list
	"Sorry. Nothing more today!"
	<p/>
	(<a href=? (urlstring (handler-url 'administration-handler)) >
	    "Back to the administration menu.")))
      (t
       (list
	(format-paste paste (paste-display-url paste) (paste-number paste))
	(loop for a in (paste-annotations paste)
	      collect (<p> (format-paste a nil (paste-number a) t)))
	(when (paste-deletion-requested paste)
	  (list
	   "Deletion requested for the following reason:"
	   (<pre class="paste-area"> (paste-deletion-requested paste))
	   "Contact email: "
	   (<a href=? (format nil "mailto:~A" (paste-deletion-requested-email paste)) >
	       (paste-deletion-requested-email paste))))
	(<h2> "Disposition:")
	(form-html (spam-review-action (handler-url 'spam-review-handler))
	  (paste paste)
	  (action :keep-paste))
	<p/>
	(<a href=? (urlstring (handler-url 'administration-handler)) >
	    "Back to the administration menu."))))))

(define-application-handler (spam-review-handler :get request)
  (application-page ("Spam Review")
    (spam-review)))

(define-application-handler (spam-review-handler :post request)
  (application-process-form (spam-review-action)
    (application-page ("Spam Review")
      (ecase action
	(:keep-paste
	 (setf (paste-maybe-spam-p paste) nil)
	 (setf (paste-deletion-requested paste) nil)
	 (paste-write-xml-to-file paste)
	 (list "The paste "
	       (<a href=? (paste-display-url paste)>
		   (prin1-to-string (paste-number paste)))
	       " has been marked as not spam."))
	(:kill-paste
	 (kill-paste (paste-number paste))
	 (list "The paste "
	       (<a href=? (paste-display-url paste)>
		   (prin1-to-string (paste-number paste)))
	       " has been deleted.")))
      <p/>
      (spam-review))))

(define-application-handler (spam-review-required-handler :get request)
  (request-send-headers request :expires 0 :content-type "text/plain")
  (format (request-stream request)
	  (if (find-spammy-paste)
	      "YES"
	      "NO"))
  t)
