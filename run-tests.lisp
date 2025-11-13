(require 'asdf)

(load "init-build-env.lisp")

(asdf:test-system "mahogany")
