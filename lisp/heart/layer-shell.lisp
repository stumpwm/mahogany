(in-package #:hrt)

;; (declaim (inline layer-surface-output))
(defun layer-surface-output (layer-surface)
  (declare (type cffi:foreign-pointer layer-surface))
  (let ((output (hrt-layer-surface-output layer-surface)))
    (if (cffi:null-pointer-p output)
        nil
        output)))
