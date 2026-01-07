(in-package :mahogany/tree)

(defun reconfigure-node (node hrt-output)
  (declare (type output-node node)
           (type cffi:foreign-pointer hrt-output))
  (multiple-value-bind (x y width height) (hrt:output-usable-area hrt-output)
    (set-position node x y)
    (set-dimensions node width height)))

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
