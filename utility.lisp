;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :lisppaste)

(defun hex-hash-to-vector (hashed)
  (if (not (eql (length hashed) (* 2 (digest-length :ripemd-160))))
      (error "Sorry; the hashed password is the wrong length."))
  (map '(vector t)
       #'identity
       (loop for byte-start from 0 to (1- (length hashed)) by 2
	  collect (parse-integer hashed :start byte-start :end (+ byte-start 2) :radix 16))))

(defun hash-password-to-vector (password)
  (coerce
   (digest-sequence :ripemd-160
		    (map '(vector (unsigned-byte 8))
			 #'char-code password))
   '(vector t)))

(defun match-prefix (prefix)
  (lambda (x)
    (eql 0
         (search prefix (script-name x)))))
