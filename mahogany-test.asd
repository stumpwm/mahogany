#|
This file is a part of mahogany.

|#

(asdf:register-system-packages "prove-asdf" "prove")

(asdf:defsystem #:mahogany-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:mahogany
	       #:prove
	       #:fiasco)
  :pathname "test/"
  :components ((:test-file "tree-tests")
	       (:file "tree-tests-2")
	       (:file "keyboard-tests")
	       (:file "log-tests"))
  :description "Test System for mahogany."
  :perform (test-op :after (op c)
                    (and (uiop/package:symbol-call "FIASCO" "ALL-TESTS")
			 (funcall (intern #.(string :run) :prove) c))))
