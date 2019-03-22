(in-package #:mahogany/backend)

;; internal generic functions:
(defgeneric view-for-each-surface (view callback-func data)
  (:documentation "Call callback-func on all of the surfaces in the view.
CALLBACK-FUNC must be a c function pointer.")
  (declare (optimize (speed 3))))

(defclass backend-view (view)
  ((surface :initarg :wlr-surface
	    :reader view-surface))
  (:default-initargs
   :mapped nil))

(defclass xdg-view (backend-view)
  ((mapped :initarg :mapped
	   :accessor view-mapped)
   (map-listener :initarg :map-listener
		 :reader view-map-listener
		 :type wl_listener)
   (configure-listener :initarg :config-listener
		       :reader view-config-listener
		       :type wl_listener)
   (unmap-listener :initarg :unmap-listener
		   :reader view-unmap-listener
		   :type wl_listener)
   (destroy-listener :initarg :destroy-listener
		     :reader view-destroy-listener
		     :type wl_listener)
   (geometry :accessor view-geometry
	     :type wlr:box)))

(defun surface-at-p (view lx ly)
  "Return the surface at the given layout coordinates in the surface. If such a surface exists,
also return its x and y surface coordinates"
  (let ((view-sx (- lx (view-x view)))
	(view-sy (- ly (view-y view))))
    (wlr:xdg-surface-at (view-surface view) view-sx view-sy)))

(defmethod view-for-each-surface ((view xdg-view) callback-func data)
  (declare (type cffi:foreign-pointer callback-func data))
  (wlr:xdg-surface-for-each-surface (view-surface view) callback-func data))

(defmethod set-dimensions ((view xdg-view) width height)
  (wlr:xdg-toplevel-set-size (view-surface view) (truncate width) (truncate height)))
