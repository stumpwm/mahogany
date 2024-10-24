(fiasco:define-test-package #:mahogany-tests/tree-2
  (:use #:mahogany/tree #:mahogany/wm-interface))

(in-package #:mahogany-tests/tree-2)

(defun make-basic-tree (&key (x 0) (y 0) (width 100) (height 100))
  (let ((container (make-instance 'tree-container))
	(frame (make-instance 'view-frame :x x :y y :width width :height height)))
    (setf (frame-parent frame) container)
    (setf (root-tree container) frame)
    (values frame container)))

(defun make-tree-frame (children &key split-direction (x 0) (y 0) (width 100) (height 100))
  (let ((parent (make-instance  'tree-frame
				:x x :y y
				:width width :height height
				:split-direction split-direction)))
    (dolist (c children)
      (setf (frame-parent c) parent))
    (setf (tree-children parent) children)
    parent))

(fiasco:deftest set-position-view-frame ()
  (let ((tree (make-basic-tree :x 0 :y 0)))
    (set-position tree 100 200)
    (is (= (frame-x tree) 100))
    (is (= (frame-y tree) 200))))

(fiasco:deftest set-position-tree-frame ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 50 :height 100))
	 (child-2 (make-instance 'view-frame :x 51 :y 0 :width 50 :height 100))
	 (parent (make-tree-frame (list child-1 child-2) :split-direction :horizontal)))
    (set-position parent 100 200)
    (is (= (frame-x child-1) 100))
    (is (= (frame-y child-1) 200))
    (is (= (frame-x child-2) 151))
    (is (= (frame-y child-2) 200))))

(fiasco:deftest setf-frame-x-view-frame-sets-value ()
  (let ((tree (make-basic-tree :x 0 :y 0)))
    (setf (frame-x tree) 100)
    (is (= 100 (frame-x tree)))))

(fiasco:deftest setf-frame-y-view-frame-sets-value ()
  (let ((tree (make-basic-tree :x 0 :y 0)))
    (setf (frame-y tree) 223)
    (is (= (frame-y tree) 223))))

(fiasco:deftest setf-frame-y-tree-frame ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 50 :height 100))
	 (child-2 (make-instance 'view-frame :x 51 :y 0 :width 50 :height 100))
	 (parent (make-tree-frame (list child-1 child-2) :split-direction :horizontal)))
    (setf (frame-y parent) 100)
    (is (= (frame-x parent) 0))
    (is (= (frame-y parent) 100))
    (is (= (frame-x child-1) 0))
    (is (= (frame-y child-2) 100))
    (is (= (frame-x child-2) 51))))

(fiasco:deftest setf-frame-x-tree-frame ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 50 :height 100))
	 (child-2 (make-instance 'view-frame :x 51 :y 0 :width 50 :height 100))
	 (parent (make-tree-frame (list child-1 child-2) :split-direction :horizontal)))
    (setf (frame-x parent) 100)
    (is (= (frame-x parent) 100))
    (is (= (frame-y parent) 0))
    (is (= (frame-x child-1) 100))
    (is (= (frame-y child-2) 0))
    (is (= (frame-x child-2) 151))))

(fiasco:deftest single-frame--frame-at-test ()
  (let ((frame (make-instance 'frame :x 0 :y 0 :width 100 :height 100)))
    (is (equal frame (frame-at frame 50 50)))
    (is (not (frame-at frame -1 50)))
    (is (not (frame-at frame 50 -1)))
    (is (not (frame-at frame -1 -1)))))

(fiasco:deftest set-dimensions-frame ()
  (let ((frame (make-instance 'frame :x 0 :y 0 :width 100 :height 100)))
    (set-dimensions frame 400 200)
    (is (= (frame-width frame) 400))
    (is (= (frame-height frame) 200))))

(fiasco:deftest set-dimensions-tree-frame-width-change ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 50 :height 100))
	 (child-2 (make-instance 'view-frame :x 50 :y 0 :width 50 :height 100))
	 (parent (make-tree-frame (list child-1 child-2) :height 100 :width 100
				  :split-direction :horizontal)))
    (set-dimensions parent 200 400)
    (is (= (frame-width parent) 200))
    (is (= (frame-height parent) 400))
    (is (= (frame-width child-1) 100))
    (is (= (frame-height child-1) 400))
    (is (= (frame-width child-2) 100))
    (is (= (frame-height child-2) 400))))

(fiasco:deftest set-dimensions-tree-frame-height-change ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 100 :height 50))
	 (child-2 (make-instance 'view-frame :x 0 :y 50 :width 100 :height 50))
	 (parent (make-tree-frame (list child-1 child-2) :height 100 :width 100
				  :split-direction :vertical)))
    (set-dimensions parent 200 400)
    (is (= (frame-width parent) 200))
    (is (= (frame-height parent) 400))
    (is (= (frame-width child-1) 200))
    (is (= (frame-height child-1) 200))
    (is (= (frame-width child-2) 200))
    (is (= (frame-height child-2) 200))))

(fiasco:deftest set-dimensions-tree-frame-chidren-move-x ()
  (let* ((child-1 (make-instance 'view-frame :x 0 :y 0 :width 50 :height 100))
	 (child-2 (make-instance 'view-frame :x 50 :y 0 :width 50 :height 100))
	 (parent (make-tree-frame (list child-1 child-2) :height 100 :width 100
				  :split-direction :horizontal)))
    (set-dimensions parent 200 400)
    (is (= (frame-x parent) 0))
    (is (= (frame-y parent) 0))
    (is (= (frame-x child-1) 0))
    (is (= (frame-y child-1) 0))
    (is (= (frame-x child-2) 100))
    (is (= (frame-y child-2) 0))))

(fiasco:deftest set-dimensions-tree-frame-chidren-move-y ()
  (let* ((child-1 (make-instance 'view-frame :x 20 :y 0 :width 100 :height 50))
	 (child-2 (make-instance 'view-frame :x 20 :y 50 :width 100 :height 50))
	 (parent (make-tree-frame (list child-1 child-2) :x 20 :y 0 :height 100 :width 100
				  :split-direction :vertical)))
    (set-dimensions parent 200 400)
    (is (= (frame-x parent) 20))
    (is (= (frame-y parent) 0))
    (is (= (frame-x child-1) 20))
    (is (= (frame-y child-1) 0))
    (is (= (frame-x child-2) 20))
    (is (= (frame-y child-2) 200))))