(in-package #:mahogany/tree)

(defclass layer-shell-frame ()
  ((layer-shell :initarg layer-shell
                :initform nil
                :type cffi:foreign-pointer)
   (parent :initarg :parent
           :initform nil
           :type (or null tree-parent)
           :accessor frame-parent)
   (next :initarg next-frame
         :initform nil
         :type (or tree-node null)
         :reader frame-next)
   (prev :initarg prev-frame
         :initform nil
         :type (or tree-node null)
         :reader frame-prev)
   (focused :initarg :focused
            :reader frame-focused
            :initform nil
            :type boolean))
  (:documentation "Frame tree node that contains a layer shell surface"))

(defmethod frame-next ((frame layer-shell-frame)))

(defmethod (setf %frame-prev) (prev (frame layer-shell-frame)))

(defmethod frame-prev ((frame layer-shell-frame)))

(defmethod (setf %frame-prev) (prev (frame layer-shell-frame)))

(defmethod mark-frame-focused :after ((frame layer-shell-frame) seat))

(defmethod mark-frame-unfocused :after ((frame layer-shell-frame) seat))
