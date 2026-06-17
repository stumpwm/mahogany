(in-package #:hrt)

(declaim (inline scene-group-add-view))
(defun scene-layer-add-view (scene-layer view)
  (declare (type view view)
           (type cffi:foreign-pointer scene-layer))
  (hrt-scene-layer-add-view scene-layer (view-hrt-view view)))

(declaim (inline scene-create-fullscreen-node))
(defun scene-create-fullscreen-node (hrt-layer view output)
  (declare (type view view)
           (type output output)
           (type cffi:foreign-pointer hrt-layer))
  (let ((node (hrt-scene-create-fullscreen-node hrt-layer
                                                 (view-hrt-view view)
                                                 (output-hrt-output output))))
    (if (not (cffi:null-pointer-p node))
        node
        (error 'mahogany/util:mahogany-panic
               :text "Could not allocate fullscreen node."))))

(declaim (inline scene-fullscreen-swap))
(defun scene-fullscreen-swap (hrt-node view)
  (hrt-scene-fullscreen-swap hrt-node (view-hrt-view view)))

(defun scene-fullscreen-configure (hrt-node output)
  (declare (type output output)
           (type cffi:foreign-pointer hrt-node))
  (hrt-scene-fullscreen-configure hrt-node (output-hrt-output output)))

(defun scene-layer-create (group)
  (declare (type cffi:foreign-pointer group))
  (let ((result (hrt-scene-layer-create group)))
    (if (not (cffi:null-pointer-p result))
        result
        (error 'mahogany/util:mahogany-panic
               :text "Could not create hrt-scene-layer"))))
