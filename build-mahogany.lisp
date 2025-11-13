(require 'asdf)

(load "init-build-env.lisp")

(declaim (optimize (debug 3)))

(cl:pushnew :hrt-debug *features*)

(asdf:make "mahogany/executable")
