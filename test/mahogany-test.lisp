(defpackage mahogany-test
  (:use :cl
	:prove))

(in-package :mahogany-test)

(deftest test-test
  (plan 3)
  (ok (not (find 4 '(1 2 3))))
  (is 4 4)
  (isnt 1 #\1))

(prove:run-test 'test-test)

(finalize)
