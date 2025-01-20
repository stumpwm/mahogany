(defpackage :mahogany-test/tree
  (:use :cl :prove)
  (:local-nicknames (#:tree #:mahogany/tree)))

(in-package :mahogany-test/tree)

(defun make-tree-for-test (&key (x 0) (y 0) (width 100) (height 100))
  (let* ((container (make-instance 'tree:tree-container))
	 (frame (tree:tree-container-add container :x x :y y :width width :height height)))
    (values frame container)))

(defmacro tree-subtest (subtest-name (tree-name container-name &rest tree-args) &body body)
  `(subtest ,subtest-name
     (multiple-value-bind (,tree-name ,container-name) ,(if tree-args
							    `(make-tree-for-test ,@tree-args)
							    `(make-tree-for-test))
       ,@body)))

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

(plan 1)
(prove:run-test 'binary-split-direction)
(finalize)
