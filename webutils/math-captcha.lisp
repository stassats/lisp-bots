(defpackage :webutils.captcha (:use :cl)
            (:export :expire-captchas :generate-captcha :check-captcha))
(in-package :webutils.captcha)
(webutils::export-all :webutils.captcha)

(defvar *captchas* nil)
(defvar *captcha-counter* 0)

(defstruct (captcha (:type list))
  id answer generated-time)

(defun expire-captchas (&optional (timeout (* 60 60)))
  "Expire active captchas older than TIMEOUT seconds."
  (let ((now (get-universal-time)))
    (setf *captchas*
          (remove-if #'(lambda (c)
                         (< (+ (captcha-generated-time c) timeout)
                            now)) *captchas*))))

(defparameter *male-names*
  '("Bill" "Bob" "James" "Jim" "Eric"))

(defparameter *female-names*
  '("Mary" "Sally" "Lucy" "Lizzie" "Natalie"))

(defun random-elt (list)
  (elt list (random (length list))))

(defun random-name ()
  (ecase (random 2)
    (1 (list (random-elt *male-names*) "his" "he"))
    (0 (list (random-elt *female-names*) "her" "she"))))

(defun generate-captcha ()
  "Generate a new captcha. Returns two values: the id of the captcha and the text to present to the user."
  (let ((id (incf *captcha-counter*)))
    (ecase (random 2)
      (0
       (let* ((width (1+ (* 2 (+ (random 10) 1))))
              (ratio (+ (random 8) 2))
              (height (* width ratio))
              (area (* width height))
              (perimeter (+ (* width 2) (* height 2)))
              (name (random-name)))
         (push (make-captcha :id id :answer ratio :generated-time (get-universal-time)) *captchas*)
         (values id
                 (format nil "Farmer ~A needs to surround ~A bunny pen with a fence. The fence needs to cover ~A square meters and must be made from ~A total meters of fencing. What is the ratio of the larger of the dimensions of the bunny pen to the smaller?" (first name) (second name) area perimeter))))
      (1
       (let* ((speed1 (* 10 (1+ (* 2 (+ (random 10) 1)))))
              (speed2 (* 10 (1+ (* 2 (+ (random 10) 1)))))
              (delay (+ (random 4) 1))
              (time (+ delay (+ (random 8) 2)))
              (distance (+ (* speed1 time) (* speed2 (- time delay))))
              (name1 (random-name))
              (name2 (random-name)))
         (push (make-captcha :id id :answer time :generated-time (get-universal-time)) *captchas*)
         (values id
                 (format nil "~A starts driving due east towards ~A and travelling ~A kph. ~A starts driving due west at ~A kph ~A hours later. They start off ~A kilometers apart. How many hours after ~A started will they meet?" (first name1) (first name2) speed1 (first name2) speed2 delay distance (first name1))))))))

(defun check-captcha (id answer)
  (let ((captcha (find id *captchas* :key #'captcha-id)))
    (if captcha
        (setf *captchas* (remove captcha *captchas*))
        (return-from check-captcha nil))
    (eql answer (captcha-answer captcha))))