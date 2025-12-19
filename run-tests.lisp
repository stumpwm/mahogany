(require 'asdf)

(load "init-build-env.lisp")

(asdf:load-system "mahogany-test")

;; (asdf:test-system "mahogany")
(let ((result (fiasco:all-tests)))
  (if result
      (uiop:quit 0)
      (uiop:quit 1)))
