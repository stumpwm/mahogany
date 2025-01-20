(defpackage :mahogany-test/tree
  (:use :cl :prove)
  (:local-nicknames (#:tree #:mahogany/tree))
  (:import-from :mahogany/tree
		#:find-frame))

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

(deftest find-frame
  (multiple-value-bind (tree container) (make-tree-for-test)
    (ok (not (find-frame tree nil)))
    (ok (find-frame tree tree))
    (multiple-value-bind (frame parent) (tree:split-frame-h tree)
      (declare (ignore parent))
      (let ((result (find-frame (tree:root-tree container) frame)))
	(ok result)
	(is result frame))
      (let ((result (find-frame (tree:root-tree container) (make-instance 'tree:frame :x 0 :y 0 :width 100
								    :height 100))))
	(ok (not result))))))

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

(defstruct dimensions
  x
  y
  width
  height)

(defun save-dimensions (tree)
  (make-dimensions :x (tree:frame-x tree)
		   :y (tree:frame-y tree)
		   :width (tree:frame-width tree)
		   :height (tree:frame-height tree)))

(deftest frame-swap
  (tree-subtest "simple case" (tree container)
    (tree:split-frame-h tree :ratio (/ 1 3))
    (let* ((children (tree:tree-children (tree:root-tree container)))
	   (first-tree (first children))
	   (second-tree (second children))
	   (first-dims (save-dimensions first-tree))
	   (second-dims (save-dimensions second-tree)))
      (tree:swap-positions first-tree second-tree)
      (let* ((children (tree:tree-children (tree:root-tree container)))
      	     (first-tree2 (first children))
      	     (second-tree2 (second children)))
      	(is first-dims (save-dimensions first-tree2) :test #'equalp)
      	(is second-dims (save-dimensions second-tree2) :test #'equalp)
      	(is second-tree first-tree2 :test #'eq)
      	(is first-tree second-tree2 :test #'eq))))
  (tree-subtest "different parents" (tree container)
    (tree:split-frame-h tree :ratio (/ 1 3))
    (let* ((first-tree (tree:split-frame-h tree))
	   (children (tree:tree-children (tree:root-tree container)))
	   (second-tree (second children))
	   (first-dims (save-dimensions first-tree))
	   (second-dims (save-dimensions second-tree)))
      (tree:swap-positions first-tree second-tree)
      (let* ((children (tree:tree-children (tree:root-tree container)))
      	     (first-tree2 (second (tree:tree-children (first children))))
      	     (second-tree2 (second children)))
      	(is first-dims (save-dimensions first-tree2) :test #'equalp)
      	(is second-dims (save-dimensions second-tree2) :test #'equalp)
      	(is second-tree first-tree2 :test #'eq)
      	(is first-tree second-tree2 :test #'eq)))))

(plan 5)
(prove:run-test 'find-frame)
(prove:run-test 'poly-split-dimensions)
(prove:run-test 'binary-split-dimensions)
(prove:run-test 'binary-split-direction)
(prove:run-test 'frame-swap)
(finalize)
