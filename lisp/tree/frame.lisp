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
    (error 'invalid-operation :text "Cannot swap positions with a frame higher in the tree."))
  ;; resize the frames so they will fit in the other's position:
  (let ((tmp-x (frame-x frame1))
	(tmp-y (frame-y frame1))
	(tmp-width (frame-width frame1))
	(tmp-height (frame-height frame1)))
    (set-position frame1 (frame-x frame2) (frame-y frame2))
    (set-dimensions frame1 (frame-width frame2) (frame-height frame2))

    (set-position frame2 tmp-x tmp-y)
    (set-dimensions frame2 tmp-width tmp-height))

  (let ((frame1-parent (frame-parent frame1))
  	(frame2-parent (frame-parent frame2)))
    (swap-items (tree-children frame1-parent) frame1
    		(tree-children frame2-parent) frame2 :test #'eq)
    (setf (frame-parent frame1) frame2-parent
  	  (frame-parent frame2) frame1-parent)))

(defmethod (setf frame-x) :before (new-x (frame tree-frame))
  "Translate the child frames so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-x frame-x))
      frame
    (let ((diff (- old-x new-x)))
      (dolist (child children)
	(setf (frame-x child) (- (frame-x child) diff))))))

(defmethod (setf frame-y) :before (new-y (frame tree-frame))
  "Translate the child frames so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-y frame-y))
      frame
    (let ((diff (- old-y new-y)))
      (dolist (child children)
	(setf (frame-y child) (- (frame-y child) diff))))))

(defmethod set-position ((frame frame) x y)
  ;; Set slots directly to avoid calling the setf methods:
  (setf (slot-value frame 'x) x
	(slot-value frame 'y) y))

(defmethod set-position :before ((frame tree-frame) new-x new-y)
  (when (or (not (= (frame-x frame) new-x)) (not (= (frame-y frame) new-y)))
    (with-accessors ((children tree-children)
		     (old-x frame-x)
		     (old-y frame-y))
	frame
      (let ((x-diff (- old-x new-x))
	    (y-diff (- old-y new-y)))
	(dolist (child children)
	  (set-position child (- (frame-x child) x-diff) (- (frame-y child) y-diff)))))))

(defmethod (setf frame-height) :before (new-height (frame tree-frame))
  "Scale and shift the children so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-height frame-height))
      frame
    (let ((diff (/ new-height old-height)))
      (cond
	((eql (tree-split-direction frame) :horizontal)
	 (let ((shift (frame-y frame)))
	   (dolist (child children)
	     (let ((adjusted-height (* diff (frame-height child))))
	       (setf (frame-height child) shift)
	       (setf (frame-y child) new-y)
	       (setf shift (+ adjusted-height shift))))))
	(t
	 (dolist (child children)
	   (setf (frame-height child) (* diff (frame-height child)))))))))

(defmethod (setf frame-width) :before (new-width (frame tree-frame))
  "Scale and shift the children so that geometry is preserved"
  (with-accessors ((children tree-children)
		   (old-width frame-width))
      frame
    (let ((diff (/ new-width old-width)))
      (cond
	((eql (tree-split-direction frame) :horizontal)
	 (let ((shift (frame-x frame)))
	   (dolist (child children)
	     (let ((adjusted-width (* diff (frame-width child))))
	       (setf (frame-width child) adjusted-width)
	       (setf (frame-x child) shift)
	       (setf shift (+ adjusted-width shift))))))
	(t
	 (dolist (child children)
	   (setf (frame-width child) (* diff (frame-width child)))))))))

(defmethod set-dimensions ((frame frame) width height)
  ;; Set slots to avoid calling methods:
  (setf (slot-value frame 'width) width
	(slot-value frame 'height) height))

(defmethod set-dimensions :before ((frame tree-frame) new-width new-height)
  (with-accessors ((children tree-children)
		   (old-height frame-height)
		   (old-width frame-width))
      frame
    (let ((height-diff (/ new-height old-height))
	  (height-shift (frame-y frame))
	  (width-diff (/ new-width old-width))
	  (width-shift (frame-x frame)))
      (ecase (tree-split-direction frame)
	(:vertical ;; y changes
	 (dolist (child children)
	   (let ((adjusted-height (* height-diff (frame-height child)))
		 (adjusted-width (* width-diff (frame-width child))))
	     (setf (frame-y child) height-shift)
	     (set-dimensions child adjusted-width adjusted-height)
	     (incf height-shift adjusted-height))))
	(:horizontal ;; x changes
	 (dolist (child children)
	   (let ((adjusted-height (* height-diff (frame-height child)))
		 (adjusted-width (* width-diff (frame-width child))))
	     (setf (frame-x child) width-shift)
	     (set-dimensions child adjusted-width adjusted-height)
	     (incf width-shift adjusted-width))))))))

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
  "In FRAME's parent, swap FRAME for NEW-PARENT. Don't change the dimensions of FRAME."
  ;; TODO: also swap the parent pointer in NEW-PARENT
  (if (root-frame-p frame)
      (setf (root-tree (frame-parent frame)) new-parent)
      (replace-item (tree-children (frame-parent frame)) frame new-parent)))

(defun %replace-frame (root frame)
  "Replace ROOT with FRAME without any cleanup. Change the dimensions and position
of FRAME to those of ROOT."
  (swap-in-parent root frame)
  (setf (frame-parent frame) (frame-parent root))
  ;; don't bother with an if-statement to see which values to change:
  (set-dimensions frame (frame-width root) (frame-height root))
  (set-position frame (frame-x root) (frame-y root)))

(defun binary-split-h (frame ratio direction parent-type)
  "Split a frame in two, with the resulting parent frame of type parent-frame.
Used to initially split all frames, regardless of type."
  (declare (type frame frame)
	   (type (or number null) ratio)
	   (type (member :right :left) direction)
	   (type symbol parent-type))
  (with-accessors ((old-width frame-width)
		   (old-height frame-height)
		   (old-x frame-x)
		   (old-y frame-y))
      frame
    (let* ((new-frame-width (* old-width ratio))
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
	   (setf new-frame (make-instance 'view-frame
					  :parent new-parent
					  :width new-frame-width
					  :height old-height
					  :x (+ old-x (- old-width new-frame-width))
					  :y old-y))
	   (setf (frame-width frame) (- old-width new-frame-width))
	   (setf (tree-children new-parent) (list frame new-frame)))
	  (:left
	   (setf new-frame (make-instance 'view-frame
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
  (declare (type frame frame)
	   (type (member :top :bottom) direction)
	   (type number ratio)
	   (type symbol parent-type))
  (with-accessors ((old-width frame-width)
		   (old-height frame-height)
		   (old-x frame-x)
		   (old-y frame-y))
      frame
    (let* ((new-frame-height (* old-height ratio))
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
	   (setf new-frame (make-instance 'view-frame
					   :parent new-parent
					   :width old-width
					   :height new-frame-height
					   :x old-x
					   :y old-y))
	   (setf (frame-height frame) (- old-height new-frame-height))
	   (setf (frame-y frame) (+ old-y new-frame-height))
	   (setf (tree-children new-parent) (list new-frame frame)))
	  (:bottom
	   (setf new-frame (make-instance 'view-frame
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
  (declare (type poly-tree-frame frame)
	   (type (member :right :left) direction)
	   (type (or number null) ratio))
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
				(* ratio parent-width)
				(* parent-width (/ 1 new-num-children))))
	   (other-children-width (/ (- parent-width new-frame-width)
					      parent-children-len))
	   (new-frame) (x-adjust) (new-frame-list))
      ;; create the new frame and add it to a new frame-list:
      (ecase direction
	(:right
	 (setf new-frame (make-instance 'view-frame
          				  :parent frame
          				  :width new-frame-width
          				  :height (frame-height frame)
          				  :x (+ parent-x (- parent-width new-frame-width))
          				  :y parent-y)
	       x-adjust 0
	       ;; adding to the back, create new list so parent-children is unchanged:
	       new-frame-list (append parent-children (list new-frame))))
	(:left
	 (setf new-frame (make-instance 'view-frame
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
  (declare (type poly-tree-frame frame)
	   (type (member :top :bottom) direction))
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
				 (* ratio parent-height)
				 (* parent-height (/ 1 new-num-children))))
	   (other-children-height (/ (- parent-height new-frame-height)
				    parent-children-len))
	   (new-frame) (y-adjust) (new-frame-list))
      ;; create the new frame and add it to a new frame-list:
      (ecase direction
	(:top
	 (setf new-frame (make-instance 'view-frame
          				  :parent frame
          				  :width parent-width
          				  :height new-frame-height
          				  :x parent-x
          				  :y parent-y)
	       y-adjust 0
	       ;; adding to the back, create new list so parent-children is unchanged:
	       new-frame-list (append parent-children (list new-frame))))
	(:bottom
	 (setf new-frame (make-instance 'view-frame
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

(defun release-frames (frame &optional (cleanup-func #'identity))
  "Remove the views stored in the frame. When a frame is removed,
REMOVE-FUNC is called with one argument: the view that was removed."
  (iter (for (frame) snakes:in-generator (leafs-in frame))
	(funcall cleanup-func frame)
	;; (setf (frame-view frame) nil)
	))

(defmethod remove-frame-from-parent :after ((parent tree-frame) frame cleanup-func)
  (declare (ignore parent))
  (release-frames frame cleanup-func))

(defmethod remove-frame-from-parent ((parent poly-tree-frame) frame cleanup-func)
  (declare (ignore cleanup-func))
  (let ((new-num-children (- (length (tree-children parent)) 1)))
    (cond
      ;; if after the removal, the tree only has one child, just swap the other child with its parent:
      ((= new-num-children 1)
       (let ((other-child (find-if (lambda (x) (not (equal frame x)))
				   (tree-children parent))))
	 (%replace-frame parent other-child)))
      (t
       ;; remove the child from the parent and set the remaining childrens' dimensions:
       (setf (tree-children parent) (remove frame (tree-children parent) :test #'equal))
       (ecase (tree-split-direction parent)
	 (:horizontal (let ((new-child-width (/ (frame-width parent) new-num-children))
			    (new-x (frame-x parent)))
		      (dolist (child (tree-children parent))
			(setf (frame-width child) new-child-width
			      (frame-x child) new-x)
			(setf new-x (+ new-x new-child-width)))))
	 (:vertical  (let ((new-child-height (/ (frame-height parent) new-num-children))
			   (new-y (frame-y parent)))
			 (dolist (child (tree-children parent))
			   (setf (frame-height child) new-child-height
				 (frame-y child) new-y)
			   (setf new-y (+ new-y new-child-height))))))))))

(defmethod remove-frame-from-parent ((parent binary-tree-frame) frame cleanup-func)
  (let ((other-child (find-if (lambda (x) (not (equal x frame)))
			      (tree-children parent))))
    (%replace-frame parent other-child)))

(defmethod remove-frame-from-parent ((root tree-container) frame cleanup-func)
  (let ((tree (root-tree root)))
    (declare (type frame tree))
    (remove-frame-from-parent tree frame cleanup-func)))

(defmethod replace-frame ((root frame) frame &optional (cleanup-func #'identity))
  (unless (eql root frame)
    (funcall cleanup-func frame)
    (%replace-frame frame frame)))

(defmethod replace-frame ((root tree-frame) frame &optional (cleanup-func #'identity))
  (let ((stack (tree-children root)))
    (iter (for child = (pop stack))
	  (while child)
	  (unless (equal frame child)
	    (etypecase child
	      (tree-frame
	       (alexandria:appendf stack (tree-children child)))
	      (frame
	       (funcall cleanup-func child))))))
  (%replace-frame root frame))

;; (defgeneric replace-frame ((frame frame) frame &optional (cleanup-func #'identity))
;;   (%replace-frame root frame)
;;   (funcall cleanup-func frame))

(defmethod print-object ((object frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y)
	object
      (format stream ":w ~A :h ~A :x ~A :y ~A"
	      (round width) (round height) (round x) (round y)))))

(defmethod print-object ((object tree-frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y children split-direction)
	object
      (format stream ":w ~A :h ~A :x ~A :y ~A ~S :children ~S"
	      width height x y split-direction children))))

(defmethod find-empty-frame ((root frame))
  (iter (for (frame) snakes:in-generator (leafs-in root))
	(unless (frame-view frame)
	  (return-from find-empty-frame frame))))

(defmethod find-empty-frame ((root tree-container))
  (find-empty-frame (root-tree root)))

(defun find-first-leaf (container)
  (declare (type tree-container container))
  ;; TODO: you don't need the generator to do this:
  (iter (for (frame) snakes:in-generator (leafs-in (root-tree container)))
	(return-from find-first-leaf frame)))

(defmethod get-empty-frames ((root frame))
  (let ((empties nil))
    (iter (for (frame) snakes:in-generator (leafs-in root))
	  (unless (frame-view frame)
	    (push frame empties)))
    empties))

(defmethod get-empty-frames ((root tree-container))
  (get-empty-frames (root-tree root)))

(defun get-populated-frames (root)
  "Return a list of view-frames that contain views"
  (remove-if (lambda (x) (not (frame-view x)))
	     (snakes:generator->list (leafs-in root))))

(defun in-frame-p (frame x y)
  (declare (type frame frame))
  (with-accessors ((frame-x frame-x)
		   (frame-y frame-y)
		   (frame-width frame-width)
		   (frame-height frame-height))
      frame
    (and (<= frame-x x)
	 (<  x (+ frame-x frame-width))
	 (<= frame-y y)
	 (<  y (+ frame-y frame-height)))))

(defmethod frame-at ((root tree-frame) x y)
  (declare (type real x y))
  (dolist (cur-frame (tree-children root))
      (when (in-frame-p cur-frame x y)
	(return-from frame-at (frame-at cur-frame x y)))))

(defmethod frame-at ((frame frame) x y)
  (declare (type real x y))
  (when (in-frame-p frame x y)
    frame))
