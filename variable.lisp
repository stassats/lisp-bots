;;;; See the LICENSE file for licensing information.

;;; This is the main file to edit to customize lisppaste to your
;;; site. In particular, the main variables are at the top, which
;;; define how lisppaste knows which site it is running on, and how to
;;; generate links to itself. There are two ways to run lisppaste:
;;; naked, and behind a proxying apache. When running naked, you can
;;; leave much of this file as-is, and change *internal-http-port* and
;;; *external-http-port* to the port you want it to run on, and
;;; *paste-site-name* to the hostname it is running on.

;;; When running behind a proxy, set *internal-http-port* to the port
;;; it will listen for requests from Apache, but set
;;; *external-http-port* to 80. Set *paste-site-name* to a hostname
;;; that lisppaste is running on, and comment out the line in
;;; *paste-external-url* as indicated.

;;; There are a few other options below, but the defaults should work
;;; well.

(in-package :lisppaste)

(defparameter *internal-http-port* 8081
  "Port lisppaste's araneida will listen on for requests from Apache.")
(defparameter *external-http-port* 80
  "Port lisppaste's araneida will listen on for requests from remote clients.")

(defparameter *paste-site-name*  "paste.lisp.org"
  ;; "localhost:8080"
  "Website we are running on (used for creating links).")

(defclass lisppaste-acceptor (easy-acceptor)
  ())

(defmethod acceptor-status-message ((acceptor lisppaste-acceptor) (code (eql +http-not-found+)) &key)
  nil)

(defvar *acceptor* (make-instance 'lisppaste-acceptor
                                  :address "localhost"
                                  :port *internal-http-port*
                                  :access-log-destination (merge-pathnames "lisppaste.log"
                                                                           (user-homedir-pathname))
                                  :message-log-destination (merge-pathnames "lisppaste-error.log"
                                                                           (user-homedir-pathname))))

(defparameter *paste-external-url*
  (format nil "http://~a/" *paste-site-name*))


(defun full-url (url)
  (format nil "~a~a"
          *paste-external-url*
          (string-left-trim "/" url)))

(defvar *meme-links* nil) ; whether to link to meme IRC logs, probably
			  ; only useful for freenode's lisppaste

(defvar *irc-network-name* "Freenode") ; the name of the IRC network
                                        ; lisppaste is running on; can
                                        ; be ignored when not running
                                        ; with an IRC connection

(defparameter *owner-email* "lisppaste-requests@common-lisp.net")

(defparameter *allowed-content-types*
  '("text/plain" "image/svg+xml" "text/html" "application/xhtml+xml"))

(defparameter *paste-maximum-size* 51200) ; in bytes

(defparameter *pastes-per-page* 50) ; for the paste list

(defparameter *banned-ips*
  '("69.11.238.252" "168.143.113.138"
    "83.216.146.102"
    "206.159.155.203" "66.122.165.201"
    "165.121.144.165" "203.70.5.30"
    "195.225.176.77" "195.225.176.73"
    "210.127.227.69" "68.214.30.130"
    "81.95.146.162" "4.235.193.37"
    "212.107.116.228" "88.213.14.56"
    "88.213.62.94" "72.160.93.163"
    "68.26.33.60" "67.68.56.66"
    "24.27.92.149" "86.178.14.1"
    "64.175.43.145" "71.205.65.255"
    "64.175.43.145" "173.216.1.3"))

(defparameter *banned-paste-titles*
  '("Free Ringtones" "My Site" "Nibelan" "My homepage"
    "OWNED BY IRC.EFNET.ORG #LINUXWAREZ" "My name is bikcmp and i suck cocks"
    "IRC.HARDCHATS.COM #GNAA"))

(defparameter *banned-paste-users*
  '("RuleZ005" "Viag@Viag.com" "Anti-Goth" "Eric_Blair"))

(defparameter *banned-user-agent-regexps*
  '("^WWW-Mechanize/"))

(defparameter *banned-content-regexps*
  '("Free.*Ringtones" "(?s)a href=.*\\[url" "(?s)A href.*\\[URL" "(?s)\\[url=http.*\\].*a href" "(?s)valium.*viagra" "(?s)Xanax.*Cialis" "(?s)(<[aA] href=\" http:[^\"]+ \">[^<]+</[aA]>(\\n|\\r)*){2}" "(?s)\\[URL.*\\[URL.*\\[URL" "(?i)Cathode Ray Terrorists Proudly Bring you" "rapidshare\\.com.*rapidshare\\.com" "kls-66@hotmail" "o3y@HotMaiL\\.CoM" "OWNED BY IRC\\.EFNET\\.ORG #LINUXWAREZ" "gnaa gnaa gnaa gnaa" "My name is bikcmp and" "GayNiggerAssociationofAmerica" "(?s)FREENODE.*FUCK" "(?s)NIGGER" "(?s)#gnaa" "(?s)#GNAA" "(?s)nigger" "(?s)jews did 9/11" "(?s)random trolldb entry" "(?s)show my tits"))

(defparameter *base-path*
  (asdf:component-pathname (asdf:find-system :lisppaste)))

(defparameter *banned-ips-file*
  (merge-pathnames (make-pathname :name "banned-ips")
		   (make-pathname :directory
				  (pathname-directory *base-path*))))

(setf *banned-ips*
      (remove-duplicates
       (append *banned-ips*
	       (with-open-file (f *banned-ips-file* :direction :input)
		 (read f)))
       :test #'equal))

(defun log-file-path (name)
  (merge-pathnames (make-pathname :directory '(:relative "logs")
				  :name name)
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *ban-log-file*
  (log-file-path "ban-log"))
                                        ; where logs of attempts by
                                        ; banned users to paste go

(defparameter *event-log-file*
  (log-file-path "event-log"))
                                        ; where normal events are
                                        ; logged

(defparameter *access-log-file*
  (log-file-path "access-log"))

(defparameter *no-channel-pastes* t) ; whether to allow pastes that
				       ; don't get announced on a
				       ; channel

(defparameter *serve-source* nil)

;; once every this often, clear out the "used" captchas
(defparameter *used-captcha-release-time* (* 60 60 24))

;; These are ripemd 160-bit hashes of administrator passwords
(defparameter *administrator-passwords* '("c549ae1a463872b01add5694bd3954026ad3d565"))

;; Annotations of pastes older than this won't be allowed
(defparameter *annotate-moldy* (* 60 60 24 30))

;; Options for the expiration times of pastes
(defparameter *expiration-options*
  `(("Never expires (recommended)" . nil)
    ("One hour" . ,(* 60 60))
    ("One day" . ,(* 60 60 24))
    ("One week" . ,(* 60 60 24 7))
    ("Four weeks" . ,(* 60 60 24 7 4))))

;; You shouldn't need to edit below this line.
;; LINE

(defparameter *hashed-administrator-passwords*
  (mapcar #'hex-hash-to-vector *administrator-passwords*))

(defparameter *display-paste-url* "/display/")

(defparameter *short-paste-url* "/+")

(defparameter *new-paste-url* "/new")

(defparameter *list-paste-url* "/list")

(defparameter *submit-paste-url* "/submit")

(defparameter *rss-url* "/list.rss")

(defparameter *rss-full-url* "/list-full.rss")

(defparameter *syndication-url* "/syndication")

(defparameter *stats-url* "/stats")

(defparameter *css-url* "/lisppaste.css")

(defparameter *email-redirect-url* "/email")

(defparameter *channel-select-url* "/channels")

(defparameter *404-urls* (list "favicon.ico" "robots.txt"
	;; (puri:merge-uris " " *paste-external-url*)
	(puri:merge-uris "%20" *paste-external-url*)))

(defparameter *main-system-server-url* "/system-server/")
(defparameter *show-component-url* "/system-server/show/")

(defvar *default-nickname* "devpaste")
(defvar *default-irc-server* "chat.freenode.net")
(defvar *default-irc-server-port* 6667)
(defvar *default-channel* "#lisppaste")

(defvar *pastes* nil)
(defvar *paste-counter* 0)
(defvar *paste-lock* (bt:make-recursive-lock "paste lock"))
(defvar *channels* '("None"))

(defmacro with-paste-lock (&body body)
  `(bt:with-recursive-lock-held (*paste-lock*)
     ,@body))

(defvar *paste-file*
  (merge-pathnames "pastes.lisp-expr"
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *paste-path*
  (merge-pathnames (make-pathname :directory '(:relative "pastes"))
                   (make-pathname
                    :directory
                    (pathname-directory
                     *base-path*))))

(defparameter *times-file-root*
  (merge-pathnames (make-pathname :directory '(:relative "logs"))
		   (make-pathname
		    :directory
		    (pathname-directory
		     *base-path*))))

(defvar *boot-time* 0)

(defvar *coloring-type-defaults* (make-hash-table :test #'equalp))
