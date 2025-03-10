#|
This file is a part of mahogany.

|#

(asdf:defsystem #:mahogany-test
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
