(in-package :specbot)

(defclass specbot (irc-bot:bot)
  ()
  (:default-initargs :name "specbot"))

(defun help-message (bot destination)
  (irc-bot:send-message bot destination
                        (format nil "To use the ~A bot, say something ~
like \"database term\", ~
where database can be: ~{~a~^ ~}."
                                (nickname bot)
                                (mapcar #'spec-lookup:name spec-lookup:*specs*))))

(defun parse-message (message)
  (let ((space (position #\Space message)))
    (when space
      (loop for spec in spec-lookup:*specs*
            when (string-equal (spec-lookup:name spec)
                               message :end2 space)
            return (values spec (string-trim " " (subseq message (1+ space))))))))

(defun lookup-term (message)
  (multiple-value-bind (spec requested-term) (parse-message message)
    (when (and spec
               (plusp (length requested-term)))
      (multiple-value-bind (url term)
          (spec-lookup:lookup spec requested-term)
        (cond (url
               (if (typep term 'spec-lookup:term)
                   (let ((full-term (unless (equalp requested-term (spec-lookup:key term))
                                      (spec-lookup:key term))))
                     (format nil "~@[~a: ~]~@[~a: ~]~a"
                             full-term (spec-lookup:title term) url))
                   url))
              (term
               (format nil "Couldn't find anything for ~A."
                       requested-term)))))))

(defun process-query (bot term destination)
  (let ((result (lookup-term term)))
    (when result
      (irc-bot:send-message bot destination result))))

(defmethod irc-bot:process-message ((bot specbot) channel sender for-nick
                                    text full-text)
  (process-query bot full-text channel))

(defmethod irc-bot:process-private-message ((bot specbot) sender
                                            text full-text)
  (if (member full-text '("help" "help?") :test #'equalp)
      (help-message bot sender)
      (process-query bot text sender)))

(defmethod irc-bot:process-message-for-bot ((bot specbot) channel sender
                                            text full-text)
  (when (member text '("help" "help?") :test #'equalp)
    (help-message bot channel)))

(defmethod irc-bot:start :before ((bot specbot) &key)
  (spec-lookup:read-specifications))
