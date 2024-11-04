(fiasco:define-test-package #:mahogany-tests/ring-list
  (:use #:ring-list))

(in-package #:mahogany-tests/ring-list)

(fiasco:deftest remove-item-when-empty-returns-nil ()
  (let ((ring (make-ring-list)))
    (is (null (remove-item ring nil)))))

(fiasco:deftest remove-item-when-empty-keeps-size ()
  (let ((ring (make-ring-list)))
    (remove-item ring nil)
    (= 0 (ring-list-size ring))))

(fiasco:deftest swap-next-signals-when-empty ()
  (let ((ring (make-ring-list)))
    (fiasco:signals error
      (swap-next ring nil))))

(fiasco:deftest swap-previous-signals-when-empty ()
  (let ((ring (make-ring-list)))
    (fiasco:signals error
      (swap-previous ring nil))))

(fiasco:deftest add-item-increments-size ()
  (let ((ring (make-ring-list)))
    (add-item ring 'foo)
    (add-item ring 'bar)
    (is (= 2 (ring-list-size ring)))))

(fiasco:deftest remove-item-decrements-counter ()
  (let ((ring (make-ring-list)))
    (add-item ring 'foo)
    (remove-item ring 'foo)
    (is (= 0 (ring-list-size ring)))))
