#|
This file is a part of mahogany.

|#

(asdf:register-system-packages "prove-asdf" "prove")

(asdf:defsystem #:mahogany-test
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:mahogany
	       #:prove)
  :components ((:module "test"
			:components
			((:test-file "tree-tests"))))
  :description "Test System for mahogany."
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
