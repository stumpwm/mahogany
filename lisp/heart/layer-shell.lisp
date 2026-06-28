(in-package #:hrt)

(defstruct (layer-surface (:constructor make-layer-surface (hrt-layer-surface)))
  (hrt-layer-surface nil :type cffi:foreign-pointer))

#-HRT-DEBUG
(declaim (inline hrt-layer-surface-output))
(defun hrt-layer-surface-output (ptr)
  (declare (type cffi:foreign-pointer ptr))
  (let ((output (%hrt-layer-surface-output ptr)))
    (if (cffi:null-pointer-p output)
        nil
        output)))

#-HRT-DEBUG
(declaim (inline layer-surface-output))
(defun layer-surface-output (layer-surface)
  (declare (type layer-surface layer-surface))
  (hrt-layer-surface-output (layer-surface-hrt-layer-surface layer-surface)))

#-HRT-DEBUG
(declaim (inline layer-surface-keyboard-interactivity))
(defun layer-surface-keyboard-interactivity (layer-surface)
  (declare (type layer-surface layer-surface))
  (hrt-layer-surface-keyboard-interactivity
   (layer-surface-hrt-layer-surface layer-surface)))

#-HRT-DEBUG
(declaim (inline layer-surface-layer-layer))
(defun layer-surface-layer (layer-surface)
  (declare (type layer-surface layer-surface))
  (hrt-layer-surface-layer
   (layer-surface-hrt-layer-surface layer-surface)))
