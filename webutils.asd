;;;; Silly emacs, this is -*- Lisp -*-

(defsystem
    webutils
  :name "webutils"
  :author "Brian Mastenbrook"
  :licence "MIT"
  :description "Some handy utilities for writing web applications"
  :depends-on (:html-encode :trivial-http :split-sequence :cl-ppcre :net-telent-date :s-xml :ironclad
                            :cl-who)
  :components
  ((:module "webutils"
    :serial t
    ((:file "package")
     (:file "xml-mixed-mode" :depends-on ("package"))
     (:file "xml-pattern-matcher-early" :depends-on ("xml-mixed-mode"))
     (:file "xml-pattern-matcher" :depends-on ("xml-pattern-matcher-early"))
     (:file "xml-parse" :depends-on ("xml-pattern-matcher-early"))
     (:file "simple-html-templating" :depends-on ("package"))
     (:file "simple-serialized-classes" :depends-on ("package"))
     (:file "trackback" :depends-on ("package"))
     ;; (:file "rss" :depends-on ("package" "xml-mixed-mode"))
     (:file "simple-captcha" :depends-on ("package" "xml-mixed-mode"))
     (:file "forms" :depends-on ("package" "simple-serialized-classes" "xml-mixed-mode"))
     ;; (:file "application" :depends-on ("package" "xml-mixed-mode" "forms"))
     (:file "misc" :depends-on ("package" "application"))
     (:file "math-captcha" :depends-on ("package"))
     (:file "tables" :depends-on ("package" "forms" "misc" "xml-mixed-mode"))
     (:file "relations" :depends-on ("package" "simple-serialized-classes"))))))
