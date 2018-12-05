(in-package :mahogany/tree)

(defun replace-item (lst old-itm new-itm &key (test #'equal))
  (alexandria:if-let ((found (member old-itm lst :test test)))
    (setf (car found) new-itm)))

(defun swap-items (lst1 item1 lst2 item2 &key (test #'equal))
  (alexandria:if-let ((found1 (member item1 lst1 :test test))
		      (found2 (member item2 lst2 :test test)))
    (progn (setf (car found1) item2)
	   (setf (car found2) item1))))

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
	(tmp-width (frame-width frame1))
	(tmp-height (frame-height frame1)))
    (setf (frame-x frame1) (frame-x frame2)
	  (frame-y frame1) (frame-y frame2))
    (set-dimensions frame1 (frame-width frame2) (frame-height frame2))

    (setf (frame-x frame2) tmp-x
	  (frame-y frame2) tmp-y)
    (set-dimensions frame2 tmp-width tmp-height))

  (let ((frame1-parent (frame-parent frame1))
  	(frame2-parent (frame-parent frame2)))
    (swap-items (tree-children frame1-parent) frame1
    		(tree-children frame2-parent) frame2 :test #'eq)
    (setf (frame-parent frame1) frame2-parent
  	  (frame-parent frame2) frame1-parent)))

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

(defmethod set-dimensions ((frame frame) width height)
  (setf (frame-width frame) width
	(frame-height frame) height))

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
    ;; (run-hook-with-args *new-frame-hook* new-frame)
    ;; (run-hook-with-args *split-frame-hook* new-frame new-parent)
    (values new-frame new-parent)))

(defmethod split-frame-v :around ((frame frame) &key ratio direction)
  "Run the appropriate hooks"
  (declare (ignore frame direction ratio))
  (multiple-value-bind (new-frame new-parent) (call-next-method)
    ;; (run-hook-with-args *new-frame-hook* new-frame)
    ;; (run-hook-with-args *split-frame-hook* new-frame new-parent)
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
	(setf (frame-parent frame) new-parent)
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
	(setf (frame-parent frame) new-parent)
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
				(truncate (* parent-width (/ 1 new-num-children)))))
	   (other-children-width (truncate (/ (- parent-width new-frame-width)
					      parent-children-len)))
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
  (assert (eql (tree-split-direction frame) :vertical))
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
				(truncate (* parent-height (/ 1 new-num-children)))))
	   (other-children-height (truncate (/ (- parent-height new-frame-height)
				    parent-children-len)))
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

(snakes:defgenerator views-in (frame)
  (if (typep frame 'tree-frame)
      (let ((stack (tree-children frame)))
	(iter (for child = (pop stack))
	      (while child)
	      (if (typep child 'tree-frame)
		  (appendf stack (tree-children child))
		  (snakes:yield child))))
      (snakes:yield frame)))

(defun release-views (frame &optional (cleanup-func #'identity))
  "Remove the views stored in the frame. When a frame is removed,
REMOVE-FUNC is called with one argument: the view that was removed."
  (iter (for (view) snakes:in-generator (views-in frame))
	(funcall cleanup-func view)
	(setf (frame-view view) nil)))

(defun promote-frame (root frame)
  (swap-in-parent root frame)
  (setf (frame-parent frame) (frame-parent root))
  ;; don't bother with an if-statement to see which values to change:
  (set-dimensions frame (frame-width root) (frame-height root))
  (setf (frame-x frame) (frame-x root)
	(frame-y frame) (frame-y root)))

(defmethod remove-frame-from-parent :after ((parent tree-frame) frame cleanup-func)
  (declare (ignore parent))
  (release-views frame cleanup-func))

(defmethod remove-frame-from-parent ((parent poly-tree-frame) frame cleanup-func)
  (declare (ignore cleanup-func))
  (let ((new-num-children (- (length (tree-children parent)) 1)))
    (cond
      ;; if after the removal, the tree only has one child, just swap the other child with its parent:
      ((= new-num-children 1)
       (let ((other-child (find-if (lambda (x) (not (equal frame x)))
				   (tree-children parent))))
	 (promote-frame parent other-child)))
      (t
       ;; remove the child from the parent and set the remaining childrens' dimensions:
       (setf (tree-children parent) (remove frame (tree-children parent) :test #'equal))
       (ecase (tree-split-direction parent)
	 (:horizontal (let ((new-child-width (truncate (/ (frame-width parent) new-num-children)))
			  (new-x (frame-x parent)))
		      (dolist (child (tree-children parent))
			(setf (frame-width child) new-child-width
			      (frame-x child) new-x)
			(setf new-x (+ new-x new-child-width)))))
	 (:vertical  (let ((new-child-height (truncate (/ (frame-height parent) new-num-children)))
			     (new-y (frame-y parent)))
			 (dolist (child (tree-children parent))
			   (setf (frame-height child) new-child-height
				 (frame-y child) new-y)
			   (setf new-y (+ new-y new-child-height))))))))))

(defmethod remove-frame-from-parent ((parent binary-tree-frame) frame cleanup-func)
  (let ((other-child (find-if (lambda (x) (not (equal x frame)))
			      (tree-children parent))))
    (promote-frame parent other-child)))


(defmethod remove-frame-from-parent ((root tree-container) frame cleanup-func)
  (let ((tree (root-tree root)))
    (declare (type frame tree))
    (remove-frame-from-parent tree frame cleanup-func)))

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
