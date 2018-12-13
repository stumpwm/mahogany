(in-package #:mahogany/backend)

;; internal generic functions:
(defgeneric view-for-each-surface (view callback-func data)
  (:documentation "Call callback-func on all of the surfaces in the view.
CALLBACK-FUNC must be a c function pointer."))

(defclass view ()
  ((surface :initarg :wlr-surface
	    :reader view-surface)
   (x :initarg :view-x
      :accessor view-x
      :initform 0
      :type integer
      :documentation "Location of surface in output coordinates")
   (y :initarg :view-y
      :accessor view-y
      :initform 0
      :type integer
      :documentation "Location of surface in output coordinates"))
  (:default-initargs
   :mapped nil))

(defclass xdg-view (view)
  ((mapped :initarg :mapped
	   :accessor view-mapped)
   (map-listener :initarg :map-listener
		 :reader view-map-listener
		 :type wl_listener)
   (unmap-listener :initarg :unmap-listener
		   :reader view-unmap-listener
		   :type wl_listener)
   (destroy-listener :initarg :destroy-listener
		     :reader view-destroy-listener
		     :type wl_listener)))


(defmethod (setf view-x) ((view view) new-x)
  (setf (slot-value view 'x) (truncate new-x)))

(defmethod (setf view-y) ((view view) new-y)
  (setf (slot-value view 'y) (truncate new-y)))

(defmethod view-for-each-surface ((view xdg-view) callback-func data)
  (declare (type cffi:foreign-pointer callback-func data))
  (wlr:xdg-surface-for-each-surface (view-surface view) callback-func data))

(defmethod set-dimensions ((view xdg-view) width height)
  (wlr:xdg-toplevel-set-size (view-surface view) (truncate width) (truncate height)))

(defmethod print-object ((object view) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (x y) object
      (format stream ":x ~S :y ~S" x y))))
