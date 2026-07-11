(in-package #:mahogany/tree)

(defclass layer-shell-frame ()
  ((layer-shell :initarg :layer-shell
                :type hrt:layer-surface)
   (parent :initarg :parent
           :initform nil
           :type (or null tree-parent)
           :accessor frame-parent)
   (next :initform nil
         :type (or tree-node null)
         :reader frame-next)
   (prev :initform nil
         :type (or tree-node null)
         :reader frame-prev)
   (focused :initarg :focused
            :reader frame-focused
            :initform nil
            :type boolean))
  (:documentation "Frame tree node that contains a layer shell surface"))

(defmethod in-frame-p ((frame layer-shell-frame) x y)
  (let ((shell (slot-value frame 'layer-shell)))
    (multiple-value-bind (frame-x frame-y)
        (hrt:layer-surface-position shell)
      (multiple-value-bind (frame-width frame-height)
          (hrt:layer-surface-dimensions shell)
        (and (<= frame-x x)
             (<  x (+ frame-x frame-width))
             (<= frame-y y)
             (<  y (+ frame-y frame-height)))))))

(defmethod frame-position ((frame layer-shell-frame))
  (let ((shell (layer-shell-frame-shell frame)))
    (hrt:layer-surface-position shell)))

(defmethod (setf %frame-prev) (prev (frame layer-shell-frame))
  (setf (slot-value frame 'prev) prev))

(defmethod (setf %frame-next) (next (frame layer-shell-frame))
  (setf (slot-value frame 'next) next))

(defmethod mark-frame-focused ((frame layer-shell-frame) seat)
  (setf (slot-value frame 'focused) t))

(defmethod mark-frame-focused :after ((frame layer-shell-frame) seat)
  (hrt::layer-surface-focus (slot-value frame 'layer-shell) seat))

(defmethod mark-frame-unfocused :after ((frame layer-shell-frame) seat)
  (hrt::layer-surface-unfocus (slot-value frame 'layer-shell) seat))

(defmethod frame-position ((frame layer-shell-frame))
  (hrt::layer-surface-position (slot-value frame 'layer-shell)))
