(asdf:load-system :minion)
(asdf:load-system :specbot)

(irc-bot:start 'minion:minion)
(irc-bot:start 'specbot:specbot)
