#|
This file is a part of mahogany.

|#

(asdf:defsystem #:mahogany-test
  :depends-on (#:mahogany
	       #:fiasco)
  :pathname "test/"
  :serial nil
  :components ((:file "util")
	       (:file "ring-list")
	       (:file "tree-tests" :depends-on ("util"))
	       (:file "keyboard-tests")
	       (:file "kmap-modes")
	       (:file "config-system-tests")
	       (:file "log-tests"))
  :description "Test System for mahogany."
  :perform (test-op :after (op c)
                    (uiop/package:symbol-call "FIASCO" "ALL-TESTS")))
