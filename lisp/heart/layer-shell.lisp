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

#-HRT-DEBUG
(declaim (inline layer-shell-surface-place))
(defun layer-shell-surface-place (hrt-layer-shell output)
  (declare (type cffi:foreign-pointer hrt-layer-shell)
           (type output output))
  (hrt-layer-shell-surface-place hrt-layer-shell
                                 (hrt:output-hrt-output output)))

#-HRT-DEBUG
(declaim (inline layer-surface-focus))
(defun layer-surface-focus (layer-surface seat)
  (declare (type layer-surface layer-shell))
  (hrt-layer-surface-focus (layer-surface-hrt-layer-surface layer-surface)
                           seat))

#-HRT-DEBUG
(declaim (inline layer-surface-unfocus))
(defun layer-surface-unfocus (layer-surface seat)
  (declare (type layer-surface layer-shell))
  (hrt-layer-surface-unfocus (layer-surface-hrt-layer-surface layer-surface)
                             seat))

#-HRT-DEBUG
(declaim (inline layer-surface-position))
(defun layer-surface-position (layer-surface)
  (declare (type layer-surface layer-shell))
  (let ((hrt-surface (layer-surface-hrt-layer-surface layer-surface)))
    (with-return-by-value ((x :int) (y :int))
      (hrt-layer-surface-position hrt-surface
                                  x y))))

#-HRT-DEBUG
(declaim (inline layer-surface-dimensions))
(defun layer-surface-dimensions (layer-surface)
  (declare (type layer-surface layer-shell))
  (let ((hrt-surface (layer-surface-hrt-layer-surface layer-surface)))
    (with-return-by-value ((width :int) (height :int))
      (hrt-layer-surface-dimensions hrt-surface
                                  width height))))
