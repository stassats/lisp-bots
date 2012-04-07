(defsystem irc-bots
  :depends-on (lisppaste specbot minion)
  :serial t
  :components ((:file "start-bots")))
