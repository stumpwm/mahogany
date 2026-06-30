(in-package #:mahogany/tree)

(defmethod frame-prev ((frame tree-parent))
  (with-slots (children) frame
    (frame-prev (first children))))

(defmethod (setf %frame-prev) (prev (frame tree-parent))
  (with-slots (children) frame
    (let ((first-child (first children)))
      (setf (%frame-prev first-child) prev))))

(defmethod (setf %frame-next) (next (frame tree-parent))
  (with-slots (children) frame
    (let ((last-child (car (last children))))
      (setf (%frame-next last-child) next))))

(defmethod frame-next ((frame tree-parent))
  (with-slots (children) frame
    (frame-next (car (last children)))))
