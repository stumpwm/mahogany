(require 'asdf)

(load "init-build-env.lisp")

;; Loading the package first makes
;; ASDF show warnings.
(asdf:load-system "mahogany")
(asdf:make "mahogany/executable")
