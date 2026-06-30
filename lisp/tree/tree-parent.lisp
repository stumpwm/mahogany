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

(defmethod find-empty-frame ((root tree-parent))
  (dolist (tree (tree-children root))
    (alexandria:when-let (empty (find-empty-frame tree))
      (return-from find-empty-frame empty)))
  nil)

(defmethod find-view-frame ((root tree-parent) view)
  (foreach-leaf (frame root)
    (alexandria:if-let ((f (find-view-frame frame view)))
      (return-from find-view-frame f)))
  nil)

(defmethod frame-at ((root tree-parent) x y)
  (declare (type real x y))
  (dolist (cur-frame (tree-children root))
    (when (in-frame-p cur-frame x y)
      (return-from frame-at (frame-at cur-frame x y)))))

(defmethod print-object ((object tree-parent) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (children)
        object
      (format stream ":children ~S" children))))
