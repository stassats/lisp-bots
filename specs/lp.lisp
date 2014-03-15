((:name "lp"
  :description "Launchpad Bugs"
  :url-prefix "https://bugs.launchpad.net/bugs/"
  :validator
    (lambda (string)
      (let ((length (length string)))
        (and (plusp length)
             (not (find-if-not #'digit-char-p string
                               :start (if (and (char= (char string 0) #\#)
                                               (> length 1))
                                          1
                                          0))))))
  :processor
    (lambda (string)
      (if (char= (char string 0) #\#)
          (subseq string 1)
          string))))
