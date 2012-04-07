(defpackage :minion
  (:use :cl :irc :cl-ppcre :split-sequence)
  (:export
   :start-minion
   :*minion-nickserv-password*
   :*respond-to-general-hellos*
   :shut-up
   :un-shut-up))

(defpackage :eliza
  (:use :cl)
  (:export :eliza))

(defpackage :steel-bazooka
  (:use :cl)
  (:export :steel-whatever))
