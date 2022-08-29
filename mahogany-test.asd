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
	       (:file "keyboard-tests"))
  :description "Test System for mahogany."
  :perform (test-op :after (op c)
                    (and (funcall (intern #.(string :run) :prove) c)
		    (uiop/package:symbol-call "FIASCO" "ALL-TESTS"))))
