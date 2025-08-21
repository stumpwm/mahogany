(in-package #:hrt)

(declaim (inline scene-group-add-view))
(defun scene-group-add-view (scene-group view)
  (declare (type view view)
	   (type cffi:foreign-pointer scene-group))
  (hrt-scene-group-add-view scene-group (view-hrt-view view)))

(declaim (inline scene-create-fullscreen-node))
(defun scene-create-fullscreen-node (hrt-group view hrt-output)
  (declare (type view view)
	   (type cffi:foreign-pointer hrt-group hrt-output))
  (alexandria:if-let ((node (hrt-scene-create-fullscreen-node hrt-group
							      (view-hrt-view view)
							      hrt-output)))
    node
    (error "Could not allocate fullscreen node.")))


(declaim (inline scene-init-view))
(defun scene-init-view (hrt-group hrt-view)
  (declare (type cffi:foreign-pointer hrt-group hrt-view))
  (hrt-scene-group-init-view hrt-group hrt-view)
  (%make-view hrt-view))
