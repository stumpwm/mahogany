#|
This file is a part of mahogany.

|#

(asdf:register-system-packages "prove-asdf" "prove")

(asdf:defsystem #:mahogany-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:mahogany
	       #:fiasco)
  :pathname "test/"
  :components ((:file "ring-list")
	       (:file "tree-tests")
	       (:file "keyboard-tests")
	       (:file "log-tests"))
  :description "Test System for mahogany."
  :perform (test-op :after (op c)
                    (uiop/package:symbol-call "FIASCO" "ALL-TESTS")))
