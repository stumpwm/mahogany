(defpackage :mahogany/tree/group
  (:use :cl :mahogany/tree/frame-interface :mahogany/tree/frames))

(in-package :mahogany/tree/group)

(defclass output-manager ()
  ((groups :initarg :groups
	   :accessor output-manager-groups
	   :type list)
   (outputs :initarg :outputs
	     :accessor output-manager-outputs
	     :type list)))

(defmethod print-object ((object group) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name)
	object
      (format stream "~A" name))))
