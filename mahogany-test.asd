#|
This file is a part of mahogany.

|#

(asdf:defsystem #:mahogany-test
    :depends-on (#:mahogany
		 #:prove)
    :defsystem-depends-on (:prove-asdf)
    :components ((:module "test"
			  :components
			  ((:test-file "mahogany-test"))))
    :description "Test System for mahogany."
     :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
