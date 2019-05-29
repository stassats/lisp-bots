;;;; See the LICENSE file for licensing information.

(asdf:defsystem :minion
  :name "minion"
  :author "Brian Mastenbrook"
  :version "0.1.0"
  :licence "MIT"
  :description "IRC bot for SBCL"
  :depends-on
  (:cl-irc :cl-ppcre :split-sequence 
   :irc-bot :drakma)
  :properties ((#:author-email . "cl-irc-devel@common-lisp.net")
               (#:date . "$Date: 2005/10/13 18:22:38 $")
               ((#:albert #:output-dir) . "doc/api-doc/")
               ((#:albert #:formats) . ("docbook"))
               ((#:albert #:docbook #:template) . "book")
               ((#:albert #:docbook #:bgcolor) . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :components ((:module "minion"
                :serial t
                :components
                ((:file "packages")
                 (:file "mp2eliza")
                 (:file "eliza-rules")
                 (:file "steel-bazooka")
                 (:file "minion")))))
