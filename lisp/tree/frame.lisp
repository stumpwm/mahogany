(in-package #:mahogany/tree)

(defun replace-item (lst old-itm new-itm &key (test #'equal))
  (alexandria:if-let ((found (member old-itm lst :test test)))
    (setf (car found) new-itm)))

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
	((eql (tree-split-direction frame) :vertical)
	 (let ((shift (frame-y frame)))
	   (dolist (child children)
	     (let ((adjusted-height (* diff (frame-height child))))
	       (setf (frame-height child) adjusted-height)
	       (setf (frame-y child) shift)
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
  (replace-item (tree-children (frame-parent frame)) frame new-parent))

(defun %replace-frame (root frame)
  "Replace ROOT with FRAME without any cleanup. Change the dimensions and position
of FRAME to those of ROOT."
  ;; check to see if we are replacing the topmost node in a tree
  ;; and the output node we are associated with has no siblings
  (if (and (root-frame-p root) (not (cdr (tree-children (frame-parent (frame-parent root))))))
      (setf (%frame-next frame) frame
	    (%frame-prev frame) frame)
      (psetf (%frame-next (frame-prev root)) frame
	     (%frame-prev (frame-next root)) frame
	     (%frame-prev frame) (frame-prev root)
	     (%frame-next frame) (frame-next root)))
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
    (let* ((new-frame-width (round (* old-width ratio)))
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
	   (setf (frame-width frame) (- old-width new-frame-width)
		 (tree-children new-parent) (list frame new-frame))
	   (psetf (%frame-prev new-frame) frame
		  (%frame-next new-frame) (frame-next frame)
		  (%frame-next frame) new-frame
		  (%frame-prev (frame-next frame)) new-frame))
	  (:left
	   (setf new-frame (make-instance 'view-frame
					  :parent new-parent
					  :width (- old-width new-frame-width)
					  :height old-height
					  :x old-x
					  :y old-y)
		 (frame-width frame) new-frame-width
		 (frame-x frame) (+ old-x new-frame-width)
		 (tree-children new-parent) (list new-frame frame))
	   (psetf (%frame-prev new-frame) (frame-prev frame)
		  (%frame-next new-frame) frame
		  (%frame-prev frame) new-frame
		  (%frame-next (frame-prev frame)) new-frame)))
	(log-string :trace "frame split new: ~S old: ~S" frame new-frame)
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
    (let* ((new-frame-height (round (* old-height ratio)))
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
	   (setf (tree-children new-parent) (list new-frame frame))
	   (psetf (%frame-prev new-frame) (frame-prev frame)
		  (%frame-next new-frame) frame
		  (%frame-prev frame) new-frame
		  (%frame-next (frame-prev frame)) new-frame))
	  (:bottom
	   (setf new-frame (make-instance 'view-frame
					   :parent new-parent
					   :width old-width
					   :height (- old-height new-frame-height)
					   :x old-x
					   :y (+ old-y (- old-height new-frame-height))))
	     (setf (frame-height frame) new-frame-height)
	     (setf (tree-children new-parent) (list frame new-frame))
	     (psetf (%frame-prev new-frame) frame
		  (%frame-next new-frame) (frame-next frame)
		  (%frame-next frame) new-frame
		  (%frame-prev (frame-next frame)) new-frame)))
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
	   (new-frame-width (round (if ratio
				       (* ratio parent-width)
				       (* parent-width (/ 1 new-num-children)))))
	   (other-children-width)
	   (new-frame) (x-adjust) (new-frame-list))
      ;; To make the total dimensions of the child frames add up to
      ;; the parent frame dimensions while still being integers, put any remainder
      ;; into the new frame:
      (multiple-value-bind (result remainder)
	  (truncate (- parent-width new-frame-width) parent-children-len)
	(setf other-children-width result
	      new-frame-width (+ new-frame-width remainder)))
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
	   (new-frame-height (round (if ratio
					(* ratio parent-height)
					(* parent-height (/ 1 new-num-children)))))
	   (other-children-height)
	   (new-frame) (y-adjust) (new-frame-list))
      ;; To make the total dimensions of the child frames add up to
      ;; the parent frame dimensions while still being integers, put any remainder
      ;; into the new frame:
      (multiple-value-bind (result remainder)
	  (truncate (- parent-width new-frame-height)
		    parent-children-len)
	(setf other-children-height result
	      new-frame-height (+ new-frame-height remainder)))
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
  (foreach-leaf (f frame)
    (funcall cleanup-func f)))

(defmethod remove-frame-from-parent :before (parent (frame frame) cleanup-func)
  (assert (equal (frame-parent frame) parent)))

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
  ;; TODO: test me!
  (with-accessors ((tree-children tree-children)) root
    (setf (tree-children root) (remove frame (tree-children root) :test #'equal))
    (when (cdr tree-children)
      (let ((frame-prev (frame-prev frame))
	    (frame-next (frame-next frame)))
	(setf (%frame-next frame-prev) frame-next
	      (%frame-prev frame-next) frame-prev)))))

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

(defmethod frame-prev ((frame tree-frame))
  (with-slots (children) frame
    (frame-prev (first children))))

(defmethod (setf %frame-prev) (prev (frame tree-frame))
  (with-slots (children) frame
    (let ((first-child (first children)))
      (setf (%frame-prev first-child) prev))))

(defmethod frame-next ((frame tree-frame))
  (with-slots (children) frame
    (frame-next (car (last children)))))

(defmethod (setf %frame-next) (next (frame tree-frame))
  (with-slots (children) frame
    (let ((last-child (car (last children))))
      (setf (%frame-next last-child) next))))

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
  (foreach-leaf (frame root)
    (unless (frame-view frame)
      (return-from find-empty-frame frame))))

(defmethod find-empty-frame ((root tree-parent))
  (dolist (tree (tree-children root))
    (alexandria:when-let (empty (find-empty-frame tree))
      (return-from find-empty-frame empty)))
  nil)

(defun find-first-leaf (tree)
  (let ((prev-frame (frame-prev tree)))
    (frame-next prev-frame)))

(defun get-populated-frames (root)
  "Return a list of view-frames that contain views"
  (let ((populated nil))
    (foreach-leaf (l root)
      (when (frame-view l)
	(push l populated)))
    populated))

;; This is really awkward, as we don't want to
;; declare an in-frame-p method for the tree-parent class,
;; as we would overshadow the implementation for the `tree-frame`
;; type, which needs to use the regular `frame` implementation:
(defmethod in-frame-p ((parent output-node) x y)
  ;; output nodes have exactly 1 child:
  (let ((f (car (tree-children parent))))
    (in-frame-p f x y)))

(defmethod frame-at ((parent output-node) x y)
  ;; output nodes have exactly 1 child:
  (let ((f (car (tree-children parent))))
    (frame-at f x y)))

(defmethod in-frame-p ((frame frame) x y)
  (declare (type real x y))
  (let ((frame-x (frame-x frame))
	(frame-y (frame-y frame))
	(frame-width (frame-width frame))
	(frame-height (frame-height frame)))
    (and (<= frame-x x)
	 (<  x (+ frame-x frame-width))
	 (<= frame-y y)
	 (<  y (+ frame-y frame-height)))))

(defmethod frame-at ((root tree-parent) x y)
  (declare (type real x y))
  (dolist (cur-frame (tree-children root))
    (when (in-frame-p cur-frame x y)
      (return-from frame-at (frame-at cur-frame x y)))))

(defmethod frame-at ((frame frame) x y)
  (declare (type real x y))
  (when (in-frame-p frame x y)
    frame))
