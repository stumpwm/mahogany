(require 'asdf)

(load "init-build-env.lisp")

(asdf:load-system "mahogany")

(mahogany::main)
