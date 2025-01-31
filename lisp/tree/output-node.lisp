(in-package :mahogany/tree)

(defmethod set-position ((frame output-node) new-x new-y)
  (set-position (first (tree-children frame)) new-x new-y))

(defmethod set-dimensions ((frame output-node) width height)
  (set-dimensions (first (tree-children frame)) width height))

(defmethod frame-prev ((frame output-node))
  (with-slots (children) frame
    (frame-prev (first children))))

(defmethod (setf %frame-prev) (prev (frame output-node))
  (with-slots (children) frame
    (let ((first-child (first children)))
      (setf (%frame-prev first-child) prev))))

(defmethod (setf %frame-next) (next (frame output-node))
  (with-slots (children) frame
    (let ((last-child (car (last children))))
      (setf (%frame-next last-child) next))))

(defmethod frame-next ((frame output-node))
  (with-slots (children) frame
    (frame-next (car (last children)))))
