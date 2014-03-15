(defpackage :specbot
  (:use :cl :irc)
  (:export
   :specbot))

(defpackage :clim-lookup
  (:use :cl :split-sequence)
  (:export
   :term-lookup
   :populate-table))

