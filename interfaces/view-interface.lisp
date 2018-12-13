(in-package #:mahogany/backend-interface)

(defgeneric set-dimensions (object width height)
  (:documentation "Set the dimensions of the the view.")
  (declare (optimize (speed 3))))

(defgeneric (setf view-x) (new-value view)
  (:documentation "Set the x coordinate of the view"))

(defgeneric (setf view-y) (new-value view)
  (:documentation "Set the y coordinate of the view"))
