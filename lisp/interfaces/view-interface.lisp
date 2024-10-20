(defpackage #:mahogany/wm-interface
  (:use :cl)
  (:export #:set-position
	   #:set-dimensions))

(in-package #:mahogany/wm-interface)

(defclass view ()
  ((x :initarg :view-x
      :reader view-x
      :initform 0
      :type fixnum
      :documentation "Location of surface in output coordinates")
   (y :initarg :view-y
      :reader view-y
      :initform 0
      :type fixnum
      :documentation "Location of surface in output coordinates")
   (opacity :initarg :opacity
	    :accessor view-opacity
	    :type single-float
	    :initform 1.0)))

(defgeneric set-dimensions (object width height)
  (:documentation "Set the dimensions of the the object.")
  (declare (optimize (speed 3))))

(defgeneric (setf view-x) (new-value view)
  (:documentation "Set the x coordinate of the view")
  (:method (new-x (view view))
    (setf (slot-value view 'x) (truncate new-x))))

(defgeneric (setf view-y) (new-value view)
  (:documentation "Set the y coordinate of the view")
  (:method (new-y (view view))
    (setf (slot-value view 'y) (truncate new-y))))

(defmethod print-object ((object view) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream ":x ~S :y ~S" x y))))

(defgeneric set-position (object x y)
  (:documentation "Set the x-y position of the object"))
