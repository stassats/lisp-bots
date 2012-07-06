(defpackage :webutils.simple-captcha
  (:use :cl webutils.xml-mixed-mode)
  (:export :make-captcha :captcha-entered-correctly-p)
  (:shadowing-import-from cl null))
(in-package :webutils.simple-captcha)
(webutils::export-all :webutils.simple-captcha)

(defstruct glyph
  (character #.(code-char 0) :type character)
  (dwx0 0 :type (signed-byte 16))
  (dwy0 0 :type (signed-byte 16))
  (bbw 0 :type (signed-byte 16))
  (bbh 0 :type (signed-byte 16))
  (bbxoff0x 0 :type (signed-byte 16))
  (bbyoff0y 0 :type (signed-byte 16))
  (bitmap (make-array '(0 0) :element-type 'bit) :type (simple-array bit (* *))))

(defvar *glyph-table* (make-hash-table :test #'eql))

(defun parse-bdf-file (file)
  (setf *glyph-table* (make-hash-table :test #'eql))
  (with-open-file (stream file)
    (let ((glyph (make-glyph)))
      (loop for line = (read-line stream nil nil)
         while line
         do (let* ((split (split-sequence:split-sequence #\space line))
                   (command (intern (first split) :keyword)))
              (case command
                (:encoding
                 (setf (glyph-character glyph)
                       (code-char (parse-integer (second split)))))
                (:dwidth
                 (destructuring-bind (dwx0 dwy0)
                     (mapcar #'parse-integer (rest split))
                   (setf (glyph-dwx0 glyph) dwx0
                         (glyph-dwy0 glyph) dwy0)))
                (:bbx
                 (destructuring-bind (bbw bbh bbxoff0x bbyoff0y)
                     (mapcar #'parse-integer (rest split))
                   (setf (glyph-bbw glyph) bbw
                         (glyph-bbh glyph) bbh
                         (glyph-bbxoff0x glyph) bbxoff0x
                         (glyph-bbyoff0y glyph) bbyoff0y
                         (glyph-bitmap glyph)
                         (make-array (list bbw bbh) :element-type 'bit))))
                (:bitmap
                 (let ((bitmap-integers
                        (loop repeat (glyph-bbh glyph)
                             collect (parse-integer (read-line stream) :radix 16)))
                       (bitmap-integer-width (1+ (floor (1- (glyph-bbw glyph)) 8))))
                   (loop for integer in bitmap-integers
                        for hindex from (1- (glyph-bbh glyph)) downto 0
                        do (let ((shifted (ash integer (- (glyph-bbw glyph)
                                                          (* 8 bitmap-integer-width)))))
                             (loop for windex from (1- (glyph-bbw glyph)) downto 0
                                  do (setf (aref (glyph-bitmap glyph) windex hindex)
                                           (mod shifted 2))
                                  (setf shifted (ash shifted -1)))))))
                (:endchar
                 (setf (gethash (glyph-character glyph) *glyph-table*)
                       glyph)
                 (setf glyph (make-glyph)))))))))

(define-symbol-macro +scaling-factor+ 2)
(defmacro scale-value (value)
  `(* ,value +scaling-factor+))

(defun randomize-list (list)
  (let ((vec (coerce list 'simple-vector)))
    (loop for i from 0 below (length vec)
         do (rotatef (elt vec i)
                     (elt vec (+ i (random (- (length vec) i))))))
    (coerce vec 'list)))

(defparameter *junk-boxes* 8)

(defun glyph->divs (glyph origin-x origin-y)
  (values
   (let ((drawn-matrix (make-array (list (glyph-bbw glyph) (glyph-bbh glyph)) :element-type 'bit :initial-element 0)))
     (loop for hindex from 0 below (glyph-bbh glyph)
        appending (loop for windex from 0 below (glyph-bbw glyph)
                     when (and (eq (aref drawn-matrix windex hindex) 0)
                               (eq (aref (glyph-bitmap glyph) windex hindex) 1))
                     collect (let ((possible-hbox (loop for ph from hindex below (glyph-bbh glyph)
                                                     while (eq (aref (glyph-bitmap glyph) windex ph) 1)
                                                     finally (return (- ph hindex))))
                                   (possible-wbox (loop for pw from windex below (glyph-bbw glyph)
                                                     while (eq (aref (glyph-bitmap glyph) pw hindex) 1)
                                                     finally (return (- pw windex))))
                                   (height 1) (width 1))
                               (cond ((and (eql possible-hbox 1)
                                           (eql possible-wbox 1))
                                      (setf (aref drawn-matrix windex hindex) 1))
                                     ((> possible-hbox possible-wbox)
                                      (loop for hoffset below possible-hbox
                                           do (setf (aref drawn-matrix windex (+ hindex hoffset)) 1))
                                      (setf height possible-hbox))
                                     (t
                                      (loop for woffset below possible-wbox
                                           do (setf (aref drawn-matrix (+ windex woffset) hindex) 1))
                                      (setf width possible-wbox)))
                               (<div style=?(format nil "width: ~Apx; height: ~Apx; bottom: ~Apx; left: ~Apx; position: absolute; background-color: ~A; z-index: ~A;"
                                                     (scale-value width)
                                                     (scale-value height)
                                                     (scale-value (+ hindex origin-y (glyph-bbyoff0y glyph)))
                                                     (scale-value (+ windex origin-x (glyph-bbxoff0x glyph)))
                                                     (random-color #xB0 #xFF)
                                                     (1+ *junk-boxes*))>)))))
   (+ origin-x (glyph-dwx0 glyph))
   origin-y))

(defun box-metrics (string)
  "Returns four values: the width and height of the box, and the x and y of the initial origin."
  (let ((width 0) (height 0) (max-height 0) (x 0) (y 0) (cursor-x 0))
    (loop for char across string
         for glyph = (gethash char *glyph-table*)
         do (setf x (min x (+ x cursor-x (glyph-bbxoff0x glyph)))
                  y (- (min (- y) (glyph-bbyoff0y glyph))))
         (setf cursor-x (max x cursor-x))
	 (setf max-height (max max-height (glyph-bbh glyph)))
         (setf width (max (+ cursor-x (glyph-dwx0 glyph))
                          (+ cursor-x (glyph-bbxoff0x glyph) (glyph-bbw glyph))))
         (setf height (max height (+ y max-height)))
         (incf cursor-x (glyph-dwx0 glyph)))
    (values width height x y)))

(defun random-char ()
  (macrolet ((random-element (literal-string)
               (check-type literal-string string)
               `(elt ',literal-string (random ,(length literal-string)))))
    (random-element "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(defun random-color (min-value max-value)
  (flet ((make-random-color-value ()
           (+ min-value (random (- (1+ max-value) min-value)))))
    (format nil "#~2,'0X~2,'0X~2,'0X"
            (make-random-color-value) (make-random-color-value) (make-random-color-value))))

(defun random-string (length)
  (let ((string (make-array (list length) :element-type 'base-char)))
    (loop for i below length
       do (setf (elt string i) (random-char)))
    string))

(defun random-key (length)
  (let ((key (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length key)
      (setf (aref key i) (random 256)))))

(defvar *key* (random-key 16))
(defvar *iv* (random-key 8))
(defvar *cipher* (ironclad:make-cipher :aes :mode :ecb :key *key* :initialization-vector *iv*))

(defun aes-encrypt-string (string pad)
  (let ((output-vector (make-array 16 :element-type '(unsigned-byte 8)))
        (input (map '(simple-array (unsigned-byte 8) (16)) #'char-code
                    (concatenate 'string string pad))))
    (ironclad:encrypt *cipher* input output-vector)
    (ironclad:byte-array-to-hex-string output-vector)))

(defun aes-decrypt-string (encrypted captcha-length)
  (let ((array (ironclad:hex-string-to-byte-array encrypted))
        (plaintext (make-array 16 :element-type '(unsigned-byte 8))))
    (ironclad:decrypt *cipher* array plaintext)
    (let ((string (map 'simple-base-string #'code-char plaintext)))
      (values (subseq string 0 captcha-length)
              (subseq string captcha-length)))))

(defun captcha-entered-correctly-p (entered captcha-length encrypted)
  "Returns true if the string ENTERED is correct given the string
ENCRYPTED which was returned by MAKE-CAPTCHA of CAPTCHA-LENGTH."
  (equal (aes-decrypt-string encrypted captcha-length) entered))

(defun make-captcha (length &key (string (random-string length)))
  "Makes a captcha of length LENGTH, or containing the text STRING of
length LENGTH. Returns two values: a XML entity containing the
captcha, and string which can be passed to
CAPTCHA-ENTERED-CORRECTLY-P."
  (unless (eql (length string) length)
    (error "Length mismatch."))
  (let* ((pad (random-string (- 16 length)))
         (encrypted (aes-encrypt-string string pad)))
    (multiple-value-bind (width height x y)
        (box-metrics string)
      (values
       (<div style=?(format nil "background-color: ~A; font-size: 0px; padding: 6px; height: ~Apx; width: ~Apx;"
                            (random-color 0 #x30)
                            (scale-value height)
                            (scale-value width))>
             (<div style=?(format nil "position: relative; height: ~Apx; width: ~Apx;"
                                  (scale-value height)
                                  (scale-value width))>
                   (randomize-list
                    (nconc
                     (map 'list (lambda (character)
                                  (multiple-value-bind (divs new-origin-x new-origin-y)
                                      (glyph->divs (gethash character *glyph-table*) x y)
                                    (setf x new-origin-x
                                          y new-origin-y)
                                    divs)) string)
                     (loop for z-index from 1 to *junk-boxes*
                        collect (let ((bottom (random height))
                                      (left (random width)))
                                  (<div style=?(format nil "position: absolute; bottom: ~Apx; left: ~Apx; height: ~Apx; width: ~Apx; background-color: ~A; z-index: ~A;"
                                                       (scale-value bottom)
                                                       (scale-value left)
                                                       (scale-value (random (- height bottom)))
                                                       (scale-value (random (- width left)))
                                                       (random-color #x10 #x40)
                                                       z-index)>)))))))
       encrypted))))

(defun write-test-html (length)
  (with-open-file (stream "captcha.html" :direction :output :if-exists :supersede)
      (xml-output-to-stream stream
                            (<html>
                             (<body>
                              (multiple-value-bind (captcha encrypted)
                                  (make-captcha length)
                                (list captcha
                                      <p/>
                                      (<tt> encrypted))))))))

(eval-when (:load-toplevel :execute)
  (parse-bdf-file (merge-pathnames (make-pathname :directory '(:relative "codec")
                                                  :name "codec-text"
                                                  :type "bdf")
                                   (asdf:component-pathname (asdf:find-system :webutils)))))
