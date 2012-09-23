(push (directory-namestring *load-truename*)
      asdf:*central-registry*)

(ql:quickload :minion)
(ql:quickload :specbot)
; (ql:quickload :lisppaste)

(irc-bot:start 'minion:minion)
(irc-bot:start 'specbot:specbot)
