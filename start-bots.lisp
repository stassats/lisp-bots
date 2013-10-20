(push (directory-namestring *load-truename*)
      asdf:*central-registry*)

(asdf:load-system :minion)
(asdf:load-system :specbot)
; (asdf:load-system :lisppaste)

(irc-bot:start 'minion:minion)
(irc-bot:start 'specbot:specbot)
