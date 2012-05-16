(defpackage :specbot
  (:use :cl :irc)
  (:export
   :start-specbot
   :shut-up
   :un-shut-up
   :specbot))

(defpackage :clim-lookup
  (:use :cl :split-sequence)
  (:export
   :term-lookup
   :populate-table))

