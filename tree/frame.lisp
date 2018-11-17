(defpackage :mahogany/tree/frames
  (:use :cl :mahogany/tree/frame-interface :alexandria)
  (:import-from :mahogany-util
		:invalid-operation))

(in-package :mahogany/tree/frames)

(defun replace-item (lst old-itm new-itm &key (test #'equal))
  (alexandria:if-let ((found (member old-itm lst :test test)))
    (setf (car found) new-itm)))

(defun find-frame (parent frame)
  (labels ((rec-func (item to-search)
	     (let ((frame (pop to-search)))
	       (cond
		 ((equal item frame)
		  (return-from rec-func item))
		 ((null frame)
		  (return-from rec-func nil))
		 ((typep frame 'tree-frame)
		  (setf to-search (append (tree-children frame)
					  to-search))))
	       (rec-func item to-search))))
    (if (equal parent frame)
	(return-from find-frame frame)
	(when (typep parent 'tree-frame)
	  (rec-func frame (tree-children parent))))))

(defmethod swap-positions ((frame1 frame) (frame2 frame))
  ;; don't swap if a frame is a parent of the other:
  (when (or (find-frame frame1 frame2) (find-frame frame2 frame1))
    (error 'invalid-operation "Cannot swap positions with a frame higher in the tree."))
  ;; resize the frames so they will fit in the other's position:
  (let ((tmp-x (frame-x frame1))
	(tmp-y (frame-y frame1))
	(tmp-wdith (frame-width frame1))
	(tmp-height (frame-height frame1)))
    (setf (frame-x frame1) (frame-x frame2)
	  (frame-y frame1) (frame-y frame2)
	  (frame-wdith frame1) (frame-wdith frame2)
	  (frame-height frame1) (frame-height frame2))
    (setf (frame-x frame2) tmp-x
	  (frame-y frame2) tmp-y
	  (frame-wdith frame2) tmp-width
	  (frame-height frame2) tmp-height))

  (let ((frame1-parent (frame-parent frame2))
	(frame2-parent (frame-parent frame2)))
    (replace-item (tree-children frame1-parent) frame1 frame2)
    (replace-item (tree-children frame2-parent) frame2 frame1)))

(defmethod (setf frame-width) :before (new-width (frame tree-frame))
  "Scale and shift the children so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-width frame-width))
      frame
    (let ((diff (/ new-width old-width))
	  (shift 0))
      (dolist (child children)
	(let ((adjusted-width (* diff (frame-width child)))
	      (new-x (+ (frame-x frame) shift)))
	  (setf (frame-width child) adjusted-width)
	  (setf (frame-x child) new-x)
	  (setf shift (+ adjusted-width shift)))))))

(defmethod (setf frame-x) :before (new-x (frame tree-frame))
  "Translate the child frames so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-x frame-x))
      frame
    (let ((diff (- old-x new-x)))
      (dolist (child children)
	(setf (frame-x child) (+ diff (frame-x child)))))))

(defmethod (setf frame-height) :before (new-height (frame tree-frame))
  "Scale and shift the children so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-height frame-height))
      frame
    (let ((diff (/ new-height old-height))
	  (shift 0))
      (dolist (child children)
	(let ((adjusted-height (* diff (frame-height child)))
	      (new-y (+ (frame-y frame) shift)))
	  (setf (frame-height child) adjusted-height)
	  (setf (frame-y child) new-y)
	  (setf shift (+ adjusted-height shift)))))))

(defmethod (setf frame-y) :before (new-y (frame tree-frame))
  "Translate the child frames so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-y frame-y))
      frame
    (let ((diff (- old-y new-y)))
      (dolist (child children)
	(setf (frame-y child) (+ diff (frame-y child)))))))

(defmethod split-frame-h :before ((frame frame) &key ratio direction)
  (declare (ignore frame direction))
  (when ratio
    (assert (> 1 ratio))))

(defmethod split-frame-v :before ((frame frame) &key ratio direction)
  (declare (ignore frame direction))
  (when ratio
    (assert (> 1 ratio))))

(defmethod split-frame-h :around ((frame frame) &key ratio direction)
  "Run the appropriate hooks"
  (declare (ignore frame direction ratio))
  (multiple-value-bind (new-frame new-parent) (call-next-method)
    (run-hook-with-args *new-frame-hook* new-frame)
    (run-hook-with-args *split-frame-hook* new-frame new-parent)
    (values new-frame new-parent)))

(defmethod split-frame-v :around ((frame frame) &key ratio direction)
  "Run the appropriate hooks"
  (declare (ignore frame direction ratio))
  (multiple-value-bind (new-frame new-parent) (call-next-method)
    (run-hook-with-args *new-frame-hook* new-frame)
    (run-hook-with-args *split-frame-hook* new-frame new-parent)
    (values new-frame new-parent)))

(defun swap-in-parent (frame new-parent)
  (if (root-frame-p frame)
      (setf (root-tree (frame-parent frame)) new-parent)
      (replace-item (tree-children (frame-parent frame)) frame new-parent)))

(defun binary-split-h (frame ratio direction parent-type)
  "Split a frame in two, with the resulting parent frame of type parent-frame.
Used to initially split all frames, regardless of type."
  (declare (type frame frame))
  (with-accessors ((old-width frame-width)
		   (old-height frame-height)
		   (old-x frame-x)
		   (old-y frame-y))
      frame
    (let* ((new-frame-width (truncate (* old-width ratio)))
	   (new-parent (make-instance parent-type
				      :split-direction :horizontal
				      :parent (frame-parent frame)
				      :width old-width
				      :height old-height
				      :x old-x
				      :y old-y)))
      ;; place the child frames:
      (let ((new-frame))
	(ecase direction
	  (:right
	   ;; TODO: change 'frame to 'view-frame:
	   (setf new-frame (make-instance 'frame
					  :parent new-parent
					  :width new-frame-width
					  :height old-height
					  :x (+ old-x (- old-width new-frame-width))
					  :y old-y))
	   (setf (frame-width frame) (- old-width new-frame-width))
	   (setf (tree-children new-parent) (list frame new-frame)))
	  (:left
	   ;; TODO: change 'frame to 'view-frame:
	   (setf new-frame (make-instance 'frame
					  :parent new-parent
					  :width (- old-width new-frame-width)
					  :height old-height
					  :x old-x
					  :y old-y))
	   (setf (frame-width frame) (- old-width new-frame-width))
	   (setf (frame-x frame) (+ old-x new-frame-width))
	   (setf (tree-children new-parent) (list new-frame frame))))
	;; insert the new node into the tree:
	(swap-in-parent frame new-parent)
	(values new-frame new-parent)))))

(defun binary-split-v (frame ratio direction parent-type)
  "Split a frame in two, with the resulting parent frame of type parent-frame.
Used to initially split all frames, regardless of type."
  (declare (type frame frame))
  (with-accessors ((old-width frame-width)
		   (old-height frame-height)
		   (old-x frame-x)
		   (old-y frame-y))
      frame
    (let* ((new-frame-height (truncate (* old-height ratio)))
	   (new-parent (make-instance parent-type
				      :split-direction :vertical
				      :parent (frame-parent frame)
				      :width old-width
				      :height old-height
				      :x old-x
				      :y old-y)))
      ;; place the child frames:
      (let ((new-frame))
	(ecase direction
	  (:top
	   ;; TODO: change 'frame to 'view-frame:
	   (setf new-frame (make-instance 'frame
					   :parent new-parent
					   :width old-width
					   :height new-frame-height
					   :x old-x
					   :y old-y))
	   (setf (frame-height frame) (- old-height new-frame-height))
	   (setf (frame-y frame) (+ old-y new-frame-height))
	   (setf (tree-children new-parent) (list new-frame frame)))
	  (:bottom
	   ;; TODO: change 'frame to 'view-frame:
	   (setf new-frame (make-instance 'frame
					   :parent new-parent
					   :width old-width
					   :height (- old-height new-frame-height)
					   :x old-x
					   :y (+ old-y (- old-height new-frame-height))))
	     (setf (frame-height frame) (- old-height new-frame-height))
	     (setf (tree-children new-parent) (list frame new-frame))))
	;; insert the new node into the tree:
	(swap-in-parent frame new-parent)
	(values new-frame new-parent)))))

(defun poly-split-frame-h (frame ratio direction)
  "Add another child to a horizontally oriented poly-tree-frame"
  (declare (type poly-tree-frame frame))
  (assert (eql (tree-split-direction frame) :horizontal))
  ;; we alread have children, so add and re-adjust:
  (with-accessors ((parent-width frame-width)
		   (parent-height frame-height)
		   (parent-x frame-x)
		   (parent-y frame-y)
		   (parent-children tree-children))
      frame
    (let* ((parent-children-len (length parent-children))
	   (new-num-children (+ parent-children-len 1))
	   (new-frame-width (if ratio
				(truncate (* ratio parent-width))
				(/ 1 new-num-children)))
	   (other-children-width (/ (- parent-width new-frame-width)
				    parent-children-len))
	   (new-frame) (x-adjust) (new-frame-list))
      ;; create the new frame and add it to a new frame-list:
      (ecase direction
	(:right     ;; TODO: change this to view-frame
	 (setf new-frame (make-instance 'frame
          				  :parent frame
          				  :width new-frame-width
          				  :height (frame-height frame)
          				  :x (+ parent-x (- parent-width new-frame-width))
          				  :y parent-y)
	       x-adjust 0
	       ;; adding to the back, create new list so parent-children is unchanged:
	       new-frame-list (append parent-children (list new-frame))))
	(:left ;; TODO: changes this to view-frame
	 (setf new-frame (make-instance 'frame
          				  :parent frame
          				  :width new-frame-width
          				  :height (frame-height frame)
          				  :x parent-x
          				  :y parent-y)
	       x-adjust (+ parent-x new-frame-width)
	       ;; we can still use parent-children to access all frames that were already there,
	       ;; as we add to the front of the list:
	       new-frame-list (cons new-frame parent-children))))
      ;; adjust the older child frames:
      (dolist (child parent-children)
	(setf (frame-width child) other-children-width)
	(setf (frame-x child) x-adjust)
	(setf x-adjust (+ x-adjust other-children-width)))
      ;; finally, set the new children list:
      (setf (tree-children frame) new-frame-list)
      (values new-frame frame))))

(defun poly-split-frame-v (frame ratio direction)
  "Add another child to a horizontally oriented poly-tree-frame"
  (declare (type poly-tree-frame frame))
  (assert (eql (tree-split-direction frame) :horizontal))
  ;; we alread have children, so add and re-adjust:
  (with-accessors ((parent-width frame-width)
		   (parent-height frame-height)
		   (parent-x frame-x)
		   (parent-y frame-y)
		   (parent-children tree-children))
      frame
    (let* ((parent-children-len (length parent-children))
	   (new-num-children (+ parent-children-len 1))
	   (new-frame-height (if ratio
				(truncate (* ratio parent-height))
				(/ 1 new-num-children)))
	   (other-children-height (/ (- parent-height new-frame-height)
				    parent-children-len))
	   (new-frame) (y-adjust) (new-frame-list))
      ;; create the new frame and add it to a new frame-list:
      (ecase direction
	(:top     ;; TODO: change this to view-frame
	 (setf new-frame (make-instance 'frame
          				  :parent frame
          				  :width parent-width
          				  :height new-frame-height
          				  :x parent-x
          				  :y parent-y)
	       y-adjust 0
	       ;; adding to the back, create new list so parent-children is unchanged:
	       new-frame-list (append parent-children (list new-frame))))
	(:bottom ;; TODO: changes this to view-frame
	 (setf new-frame (make-instance 'frame
          				  :parent frame
          				  :width parent-width
          				  :height new-frame-height
          				  :x parent-x
          				  :y (+ parent-y (- parent-height new-frame-height)))
	       y-adjust (+ parent-x new-frame-height)
	       ;; we can still use parent-children to access all frames that were already there,
	       ;; as we add to the front of the list:
	       new-frame-list (cons new-frame parent-children))))
      ;; adjust the older child frames:
      (dolist (child parent-children)
	(setf (frame-height child) other-children-height)
	(setf (frame-y child) y-adjust)
	(setf y-adjust (+ y-adjust other-children-height)))
      ;; finally, set the new children list:
      (setf (tree-children frame) new-frame-list)
      (values new-frame frame))))

(defun decode-new-split-type ()
  (ecase *new-split-type*
    (:binary 'binary-tree-frame)
    (:many 'poly-tree-frame)))

(defmethod split-frame-h ((frame frame) &key (ratio 1/2) (direction :right))
  ;; an unsplit frame splits the same way the first time:
  (binary-split-h frame ratio direction (decode-new-split-type)))

(defmethod split-frame-v ((frame frame) &key (ratio 1/2) (direction :bottom))
  ;; an unsplit frame splits the same way the first time:
  (binary-split-v frame ratio direction (decode-new-split-type)))

(defmethod split-frame-h ((frame binary-tree-frame) &key (ratio 1/2) (direction :right))
  (binary-split-h frame ratio direction 'binary-tree-frame))

(defmethod split-frame-v ((frame binary-tree-frame) &key (ratio 1/2) (direction :bottom))
  (binary-split-v frame ratio direction 'binary-tree-frame))

(defmethod split-frame-h ((frame poly-tree-frame) &key ratio (direction :right))
  "Add another child to the frame."
  (if (eql (tree-split-direction frame) :horizontal)
      (poly-split-frame-h frame ratio direction)
      (binary-split-h frame ratio direction (decode-new-split-type))))

(defmethod split-frame-v ((frame poly-tree-frame) &key ratio (direction :bottom))
  "Add another child to the frame."
  (if (eql (tree-split-direction frame) :vertical)
      (poly-split-frame-v frame ratio direction)
      (binary-split-v frame ratio direction (decode-new-split-type))))

(defmethod print-object ((object frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y)
	object
      (format stream ":w ~A :h ~A :x ~A :y ~A"
	      width height x y))))

(defmethod print-object ((object tree-frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y children split-direction)
	object
      (format stream ":w ~A :h ~A :x ~A :y ~A ~S :children ~S"
	      width height x y split-direction children))))
