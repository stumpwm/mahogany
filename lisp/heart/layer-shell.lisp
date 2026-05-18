(in-package #:hrt)

(defstruct (layer-surface (:constructor make-layer-surface (hrt-layer-surface)))
  (hrt-layer-surface nil :type cffi:foreign-pointer))

;; (declaim (inline layer-surface-output))
(defun layer-surface-output (layer-surface)
  (declare (type cffi:foreign-pointer layer-surface))
  (let ((output (hrt-layer-surface-output layer-surface)))
    (if (cffi:null-pointer-p output)
        nil
        output)))
