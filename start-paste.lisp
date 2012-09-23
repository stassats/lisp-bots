(push (directory-namestring *load-truename*)
      asdf:*central-registry*)

(ql:quickload :lisppaste)

(lisppaste:start-lisppaste)
