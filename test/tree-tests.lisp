(defpackage :mahogany-test/tree
  (:use :cl :prove :mahogany/tree)
  (:import-from :mahogany/tree
		#:find-frame))

(in-package :mahogany-test/tree)

(defmacro tree-subtest (subtest-name (tree-name container-name &rest tree-args) &body body)
  `(subtest ,subtest-name
     (multiple-value-bind (,tree-name ,container-name) ,(if tree-args
							    `(make-basic-tree ,@tree-args)
							    `(make-basic-tree))
       ,@body)))

(defun make-basic-tree (&key (x 0) (y 0) (width 100) (height 100))
  (let ((container (make-instance 'tree-container))
	(frame (make-instance 'view-frame :x x :y y :width width :height height)))
    (setf (frame-parent frame) container)
    (setf (root-tree container) frame)
    (values frame container)))

(deftest find-frame
  (multiple-value-bind (tree container) (make-basic-tree)
    (ok (not (find-frame tree nil)))
    (ok (find-frame tree tree))
    (multiple-value-bind (frame parent) (split-frame-h tree)
      (declare (ignore parent))
      (let ((result (find-frame (root-tree container) frame)))
	(ok result)
	(is result frame))
      (let ((result (find-frame (root-tree container) (make-instance 'frame :x 0 :y 0 :width 100
								    :height 100))))
	(ok (not result))))))

(defun check-size (ratio original-size frames accessor)
  (let ((proper-size (truncate (* ratio original-size)))
	(sum 0))
    (dolist (item frames)
      (setf sum (+ sum (funcall accessor item)))
      (is proper-size (funcall accessor item)))
    (diag "Check if full screen is being used:")
    (skip 1 "Not implemented yet")))
    ;;(is sum original-size)))

(defun poly-split-dim-test (repeats container split-function accessor)
  (dotimes (i repeats)
    (let ((ratio (/ 1 (+ 2 i))))
      (diag (format nil "Testing ~A split" ratio))
      (funcall split-function (root-tree container))
      (check-size ratio (funcall accessor (root-tree container))
		  (tree-children (root-tree container)) accessor))))

(deftest poly-split-dimensions
  (let ((*new-split-type* :many))
    (tree-subtest "Vertical split" (tree container)
      (declare (ignore tree))
      (poly-split-dim-test 4 container #'split-frame-v #'frame-height))
    (tree-subtest "Horizontal split" (tree container)
      (declare (ignore tree))
      (poly-split-dim-test 4 container #'split-frame-h #'frame-width))))

(deftest binary-split-dimensions
  (let ((*new-split-type* :binary))
    (tree-subtest "Vertical split" (tree container)
      (declare (ignore tree))
      (split-frame-v (root-tree container))
      (check-size 1/2 (frame-height (root-tree container))
		  (tree-children (root-tree container))
		  #'frame-height))
    (tree-subtest "Horizontal split" (tree container)
      (declare (ignore tree))
      (split-frame-h (root-tree container))
      (check-size 1/2 (frame-width (root-tree container))
		  (tree-children (root-tree container))
		  #'frame-width))))

(defstruct dimensions
  x
  y
  width
  height)

(defun save-dimensions (tree)
  (make-dimensions :x (frame-x tree)
		   :y (frame-y tree)
		   :width (frame-width tree)
		   :height (frame-height tree)))

(deftest frame-swap
  (tree-subtest "simple case" (tree container)
    (split-frame-h tree :ratio (/ 1 3))
    ;; TODO: actually write this test
    (let* ((children (tree-children (root-tree container)))
	   (first-tree (first children))
	   (second-tree (second children))
	   (first-dims (save-dimensions first-tree))
	   (second-dims (save-dimensions second-tree)))
      (swap-positions first-tree second-tree)
      (let* ((children (tree-children (root-tree container)))
      	     (first-tree2 (first children))
      	     (second-tree2 (second children)))
      	(is first-dims (save-dimensions first-tree2) :test #'equalp)
      	(is second-dims (save-dimensions second-tree2) :test #'equalp)
      	(is second-tree first-tree2 :test #'eq)
      	(is first-tree second-tree2 :test #'eq))))
  (tree-subtest "different parents" (tree container)
    ;; (print (root-tree container))
    (split-frame-h tree :ratio (/ 1 3))
    ;; TODO: actually write this test
    (let* ((first-tree (split-frame-h tree))
	   (children (tree-children (root-tree container)))
	   (second-tree (second children))
	   (first-dims (save-dimensions first-tree))
	   (second-dims (save-dimensions second-tree)))
      (swap-positions first-tree second-tree)
      (let* ((children (tree-children (root-tree container)))
      	     (first-tree2 (second (tree-children (first children))))
      	     (second-tree2 (second children)))
      	(is first-dims (save-dimensions first-tree2) :test #'equalp)
      	(is second-dims (save-dimensions second-tree2) :test #'equalp)
      	(is second-tree first-tree2 :test #'eq)
      	(is first-tree second-tree2 :test #'eq)))))

(plan 4)
(prove:run-test 'find-frame)
(prove:run-test 'poly-split-dimensions)
(prove:run-test 'binary-split-dimensions)
(prove:run-test 'frame-swap)
(finalize)
