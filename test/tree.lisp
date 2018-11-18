(defpackage :mahogany-test/tree
  (:use :cl :prove :mahogany/tree)
  (:import-from :mahogany/tree
		#:find-frame))

(in-package :mahogany-test/tree)

;; (setf prove:*default-reporter* :dot)

(defmacro tree-subtest (subtest-name (tree-name container-name &rest tree-args) &body body)
  `(subtest ,subtest-name
     (multiple-value-bind (,tree-name ,container-name) ,(if tree-args
							    `(make-basic-tree ,@tree-args)
							    `(make-basic-tree))
       ,@body)))

(defun make-basic-tree (&key (x 0) (y 0) (width 100) (height 100))
  (let ((container (make-instance 'tree-container))
	(frame (make-instance 'frame :x x :y y :width width :height height)))
    (setf (frame-parent frame) container)
    (setf (root-tree container) frame)
    (values frame container)))

(deftest find-frame
  (multiple-value-bind (tree container) (make-basic-tree)
    (diag "Testing single frame")
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
      (is proper-size (funcall accessor item)))))
    ;; (diag "Check if full screen is being used:")
    ;; (is sum original-size)))

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

(plan 3)
(prove:run-test 'find-frame)
(prove:run-test 'poly-split-dimensions)
(prove:run-test 'binary-split-dimensions)
(finalize)
