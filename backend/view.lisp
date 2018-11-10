(in-package :mahogany/backend)

;; external generic functions:
(defgeneric set-dimensions (view width height)
  (:documentation "Set the width and height of the view"))

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
      :documentation "Location of surface in output coordinates")))

(defclass xdg-view (view)
  ((map-listener :initarg :map-listener
		 :reader view-map-listener
		 :type wl_listener)
   (unmap-listener :initarg :unmap-listener
		   :reader view-unmap-listener
		   :type wl_listener)
   (destroy-listener :initarg :destroy-listener
		     :reader view-destroy-listener
		     :type wl_listener)))

(defmethod view-for-each-surface ((view xdg-view) callback-func data)
  (declare (type cffi:foreign-pointer callback-func data))
  (wlr:xdg-surface-for-each-surface (view-surface view) callback-func data))

(defmethod set-dimensions ((view xdg-view) width height)
  (wlr:xdg-toplevel-set-size (view-surface view) width height))
