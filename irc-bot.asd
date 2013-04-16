(defsystem irc-bot
  :depends-on (cl-irc cl-ppcre
               bordeaux-threads
               iolib
               usocket)
  :components ((:module "irc-bot"
                :serial t
                :components
                ((:file "packages")
                 (:file "connection")))))
