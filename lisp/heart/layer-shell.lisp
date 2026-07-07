(in-package #:hrt)

;; (declaim (inline layer-surface-output))
(defun layer-surface-output (layer-surface)
  (declare (type cffi:foreign-pointer layer-surface))
  (let ((output (hrt-layer-surface-output layer-surface)))
    (if (cffi:null-pointer-p output)
        nil
        output)))

#-HRT-DEBUG
(declaim (inline layer-shell-arrange-layers))
(defun layer-shell-arrange-layers (output &optional (emit-event t))
  (declare (type output output))
  (let ((output (output-hrt-output output)))
    (hrt-layer-shell-arrange-layers output emit-event)))
