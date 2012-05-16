(defpackage :irc-bot
  (:use :cl :cl-irc)
  (:export #:start-bots
           #:process-message-for-bot
           #:process-private-message
           #:process-message
           #:send-message
           #:bot
           #:name
           #:nickname
           #:password
           #:server
           #:channels
           #:connection
           #:thread
           #:nick-retry-count
           #:ping-timeout
           #:*ping-timeout*
           #:*debug*
           #:*bot*
           #:stop
           #:setup-connection
           #:start
           #:process-all-messages
           #:initialize-bot-from-config))
