(in-package :mahogany/tree)

(defun reconfigure-node (node output)
  (declare (type output-node node)
           (type hrt:output output))
  (multiple-value-bind (x y width height) (hrt:output-usable-area output)
    (log-string :trace "Reconfiguring output node ~S to (~S,~S) width: ~S height: ~S"
                (hrt:output-full-name output) x y width height)
    (set-position (first (tree-children node)) x y)
    (set-dimensions (first (tree-children node)) width height))
  (alexandria:when-let ((fullscreen-node (output-node-fullscreen node)))
    (hrt:scene-fullscreen-configure (%fullscreen-data-node fullscreen-node) output)))

;; Don't implement set-position and set-dimensions, as there are two differen
;;  dimensions we need to worry about; the usable area for the tilted layers
;;  and the full area for the fullscreen windows. These methods don't differentiate.

(defun set-fullscreen (output-node view)
  "Set the given view to fullscreen on this output and return the previously fullscreened
view, if there was one."
  (declare (type output-node output-node))
  ;; Re-arrange the next and prev pointers to point to this node instead of one its children:
  (let ((prev (frame-prev output-node))
        (next (frame-next output-node)))
    (setf (%frame-next prev) output-node
          (%frame-prev next) output-node))
  (log-string :trace "~@<Making output node fullscreen:~I ~:_~S ~:_on layer ~S~:>"
              output-node (layer-container-layer (frame-parent output-node)))
  (let* ((container (output-node-output output-node))
         (output (output-container-output container))
         (fullscreen (output-node-fullscreen output-node)))
    (alexandria:if-let ((fullscreen-data fullscreen))
      ;; There's already a fullscreen item, swap the new one in:
      (let ((fullscreen-node (%fullscreen-data-node fullscreen-data))
            (other-view (%fullscreen-data-view fullscreen-data)))
        (hrt:scene-fullscreen-swap fullscreen-node view)
        (hrt:scene-fullscreen-configure fullscreen-node output)
        (setf (%fullscreen-data-view fullscreen-data) view)
        other-view)
      (let* ((hrt-layer (layer-container-layer (frame-parent output-node)))
             (fullscreen-node (hrt:scene-create-fullscreen-node hrt-layer view output)))
        (log-string :trace "Created new fullscreen node for output")
        (setf (output-node-fullscreen output-node) (%make-fullscreen-data view fullscreen-node))
        nil))))

(defun %find-first-child (frame)
  (declare (type tree-parent frame))
  (let ((f frame))
    (loop :until (typep f 'view-frame)
          :do (setf f (first (tree-children f))))
    f))

(defun %find-last-child (frame)
  (declare (type tree-parent frame))
  (let ((f frame))
    (loop :until (typep f 'view-frame)
          :do (setf f (car (last (tree-children f)))))
    f))

(defun clear-fullscreen (output-node)
  (declare (type output-node output-node))
  ;; reset the next and prev pointers to point to a child frame:
  (let ((prev (frame-prev output-node))
        (next (frame-next output-node)))
    (setf (%frame-next prev) (%find-first-child output-node)
	  (%frame-prev next) (%find-last-child output-node)))
  (alexandria:when-let ((data (output-node-fullscreen output-node)))
    (let ((node (%fullscreen-data-node data))
          (view (%fullscreen-data-view data)))
      (hrt:hrt-scene-fullscreen-node-destroy node)
      (setf (output-node-fullscreen output-node) nil)
      view)))

(defmethod mark-frame-focused :after ((frame output-node) seat)
  (alexandria:when-let ((data (output-node-fullscreen frame)))
    (log-string :trace "fullscreen frame focused ~S" frame)
    (hrt:focus-view (%fullscreen-data-view data) seat)))

(defmethod unmark-frame-focused :after ((frame output-node) seat)
  (alexandria:when-let ((data (output-node-fullscreen frame)))
    (log-string :trace "fullscreen frame unfocused ~S" frame)
    (hrt:unfocus-view (%fullscreen-data-view data) seat)))

(defmethod frame-view ((frame output-node))
  (alexandria:when-let ((data (output-node-fullscreen frame)))
    (%fullscreen-data-view data)))

(defmethod in-frame-p ((parent output-node) x y)
  ;; Use the dimensions and position of the output:
  (declare (type real x y))
  (let* ((container (output-node-output parent))
         (output (output-container-output container)))
    (multiple-value-bind (frame-width frame-height) (hrt:output-resolution output)
      (multiple-value-bind (frame-x frame-y) (hrt:output-position output)
        (and (<= frame-x x)
             (<  x (+ frame-x frame-width))
             (<= frame-y y)
             (<  y (+ frame-y frame-height)))))))

(defmethod frame-at ((parent output-node) x y)
  ;; If there is a fullscreen window, return this node:
  (if (output-node-fullscreen parent)
      parent
      ;; output nodes have exactly 1 child:
      (let ((f (car (tree-children parent))))
        (frame-at f x y))))

(defmethod find-view-frame ((node output-node) view)
  (alexandria:if-let ((data (output-node-fullscreen node)))
    (when (equal (%fullscreen-data-view data) view)
      node)
    (call-next-method)))

(defmethod root-frame-p ((frame output-node))
  t)

(defmethod print-object ((object output-node) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (children output fullscreen)
        object
      (format stream
              "~:_:output ~S ~:_:fullscreen ~S ~:_:children ~S"
              output fullscreen children))))
