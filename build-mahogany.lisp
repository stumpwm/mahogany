(require 'asdf)

(load "init-build-env.lisp")

(asdf:make "mahogany/executable")
