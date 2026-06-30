(in-package #:mahogany/tree)

(defclass output-container ()
  ((hrt-output :initarg :output
               :reader output-container-output
               :type hrt:output)
   (background-layer :initarg :background
                     :type layer-container
                     :reader output-container-background)
   (bottom-layer :initarg :bottom
                 :type layer-container
                 :reader output-container-bottom)
   (top-layer :initarg :top
              :type layer-container
              :reader output-container-top)
   (overlay-layer :initarg :overlay
                  :type layer-container
                  :reader output-container-overlay)))

(declaim (inline output-container-output-ptr))
(defun output-container-output-ptr (container)
  (declare (type output-container container))
  (hrt:output-hrt-output (output-container-output container)))

(defun make-output-container (output)
  (declare (type hrt:output output))
  (let ((output-scene (hrt:output-scene output)))
    (macrolet ((make-layer (layer)
                 ;; We use a macro so we can open code the access to the layer:
                 `(make-instance 'layer-container
                                 :hrt-layer (hrt:output-scene-layer output-scene
                                                                    ,layer))))
      (make-instance 'output-container
                     :output output
                     :background (make-layer :layer-background)
                     :bottom (make-layer :layer-bottom)
                     :top (make-layer :layer-top)
                     :overlay (make-layer :layer-overlay)))))
