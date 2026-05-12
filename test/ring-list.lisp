(fiasco:define-test-package #:mahogany-tests/ring-list
  (:use #:ring-list))

(in-package #:mahogany-tests/ring-list)

(fiasco:deftest remove-item-when-empty-returns-nil ()
  (let ((ring (make-ring-list)))
    (is (null (remove-item ring nil)))))

(fiasco:deftest remove-item-when-empty-keeps-size ()
  (let ((ring (make-ring-list)))
    (remove-item ring nil)
    (is (= 0 (ring-list-size ring)))))

(fiasco:deftest pop-item-when-empty-works ()
  (let* ((ring (make-ring-list))
         (removed (pop-item ring)))
    (is (= 0 (ring-list-size ring)))
    (is (null removed))))

(fiasco:deftest pop-item-works ()
  (let ((ring (make-ring-list)))
    (add-item ring 'a)
    (add-item ring 'b)
    (add-item ring 'c)
    (let ((first (pop-item ring)))
      (is (= 2 (ring-list-size ring)))
      (is (eql 'c first)))))

(fiasco:deftest pop-item-prev-when-empty-works ()
  (let* ((ring (make-ring-list))
         (removed  (pop-item-prev ring)))
    (is (null removed))
    (is (= 0 (ring-list-size ring)))))

(fiasco:deftest pop-item-prev-works ()
  (let ((ring (make-ring-list)))
    (add-item ring 1)
    (add-item ring 2)
    (add-item ring 3)
    (let ((prev (pop-item-prev ring)))
      (is (= 1 prev))
      (is (= 2 (ring-list-size ring))))))

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

(fiasco:deftest remove-item-decrements-size ()
  (let ((ring (make-ring-list)))
    (add-item ring 'foo)
    (remove-item ring 'foo)
    (is (= 0 (ring-list-size ring)))))

(fiasco:deftest add-item-prev-when-empty-works ()
  (let* ((ring (make-ring-list)))
    (add-item-prev ring 'a)
    (is (= 1 (ring-list-size ring)))
    (is (equal 'a (peek-item ring)))))

(fiasco:deftest add-item-prev-increments-size ()
  (let ((ring (make-ring-list)))
    (add-item ring 'a)
    (add-item-prev ring 'b)
    (add-item-prev ring 'c)
    (is (= 3 (ring-list-size ring)))))

(fiasco:deftest add-item-prev-popping-with-pop-item ()
  ;; Test removing items from the tail using pop-item
  (let ((ring (make-ring-list)))
    (loop for i from 1 to 4
          do (add-item-prev ring i))
    ;; Pop items sequentially - should get 1, 2, 3, 4
    (dotimes (i 4)
      (is (= (+ 1 i) (pop-item ring))))))

(fiasco:deftest add-item-prev-popping-with-pop-item-prev ()
  ;; Test removing items from the tail using pop-item
  (let ((ring (make-ring-list)))
    (loop for i from 1 to 4
          do (add-item-prev ring i))
    (ring-list::print-backwards ring)
    (dolist (i (list 4 3 2 1))
      (is (= i (pop-item-prev ring))))))
