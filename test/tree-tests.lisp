(defpackage :mahogany-test/tree
  (:use :cl :prove)
  (:local-nicknames (#:tree #:mahogany/tree)))

(in-package :mahogany-test/tree)

(defmacro tree-subtest (subtest-name (tree-name container-name &rest tree-args) &body body)
  `(subtest ,subtest-name
     (multiple-value-bind (,tree-name ,container-name) ,(if tree-args
							    `(make-tree-for-test ,@tree-args)
							    `(make-tree-for-test))
       ,@body)))

(defun make-tree-for-test (&key (x 0) (y 0) (width 100) (height 100))
  (multiple-value-bind (container frame) (tree:make-basic-tree :x x
									:y y
									:width width
									:height height)
    (values frame container)))
(defun check-size (ratio original-size frames accessor)
  (let ((proper-size (* ratio original-size))
	(sum 0))
    (dolist (item frames)
      (setf sum (+ sum (funcall accessor item)))
      (is (funcall accessor item) proper-size))
    (diag "Check if full screen is being used:")
    (is sum original-size)))

(defun poly-split-dim-test (repeats container split-function accessor)
  (dotimes (i repeats)
    (let ((ratio (/ 1 (+ 2 i))))
      (diag (format nil "Testing ~A split" ratio))
      (funcall split-function (tree:root-tree container))
      (check-size ratio (funcall accessor (tree:root-tree container))
		  (tree:tree-children (tree:root-tree container)) accessor))))

(deftest poly-split-dimensions
  (let ((tree:*new-split-type* :many))
    (tree-subtest "Vertical split" (tree container)
		  (declare (ignore tree))
		  (poly-split-dim-test 4 container #'tree:split-frame-v #'tree:frame-height))
    (tree-subtest "Horizontal split" (tree container)
		  (declare (ignore tree))
		  (poly-split-dim-test 4 container #'tree:split-frame-h #'tree:frame-width))))

(deftest binary-split-dimensions
  (let ((tree:*new-split-type* :binary))
    (tree-subtest "Vertical split" (tree container)
      (declare (ignore tree))
      (tree:split-frame-v (tree:root-tree container))
      (check-size 1/2 (tree:frame-height (tree:root-tree container))
		  (tree:tree-children (tree:root-tree container))
		  #'tree:frame-height))
    (tree-subtest "Horizontal split" (tree container)
      (declare (ignore tree))
      (tree:split-frame-h (tree:root-tree container))
      (check-size 1/2 (tree:frame-width (tree:root-tree container))
		  (tree:tree-children (tree:root-tree container))
		  #'tree:frame-width))))

(deftest binary-split-direction
  (tree-subtest "Errors" (tree container)
    (declare (ignore tree))
    (prove:is-error (tree:split-frame-v (tree:root-tree container) :direction :left) 'error)
    (prove:is-error (tree:split-frame-h (tree:root-tree container) :direction :top) 'error))
  (subtest "vertical split"
    (diag "Testing :top")
    (multiple-value-bind (tree container) (make-tree-for-test)
      (declare (ignore container))
      (multiple-value-bind (new-frame new-parent) (tree:split-frame-v tree :direction :top)
	(is new-frame (first (tree:tree-children new-parent)))))
    (diag "Testing :bottom")
    (multiple-value-bind (tree container) (make-tree-for-test)
      (declare (ignore container))
      (multiple-value-bind (new-frame new-parent) (tree:split-frame-v tree :direction :bottom)
	(is new-frame (second (tree:tree-children new-parent))))))
  (subtest "Horizontal split"
    (diag "Testing :left")
    (multiple-value-bind (tree container) (make-tree-for-test)
      (declare (ignore container))
      (multiple-value-bind (new-frame new-parent) (tree:split-frame-h tree :direction :left)
	(is new-frame (first (tree:tree-children new-parent)))))
    (diag "Testing :right")
    (multiple-value-bind (tree container) (make-tree-for-test)
      (declare (ignore container))
      (multiple-value-bind (new-frame new-parent) (tree:split-frame-h tree :direction :right)
      (is new-frame (second (tree:tree-children new-parent)))))))

(plan 3)
(prove:run-test 'poly-split-dimensions)
(prove:run-test 'binary-split-dimensions)
(prove:run-test 'binary-split-direction)
(finalize)
