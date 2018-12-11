(in-package #:mahogany/backend)

(defclass mahogany-output ()
  ((wlr-output :initarg :wlr-output
	      :reader output-wlr-output
	      :type wlr:output)
  (frame-listener :initarg :frame-listener
		  :reader output-frame-listener
		  :type wl_listener)))

(defmethod output-width ((output mahogany-output))
  "Get the width of the output"
  (foreign-slot-value (output-wlr-output output)
		      '(:struct wlr:output)
		      :width))

(defmethod output-height ((output mahogany-output))
  "Get the width of the output"
  (foreign-slot-value (output-wlr-output output)
		      '(:struct wlr:output)
		      :height))

(defmethod output-scale ((output mahogany-output))
  "Get the scale of the output"
  (foreign-slot-value (output-wlr-output output)
		      '(:struct wlr:output)
		      :scale))

(defcstruct render-data
  (output :pointer)
  (renderer :pointer)
  (view-x :int)
  (view-y :int)
  (time (:pointer (:struct timespec))))

(declaim (inline get-surface-render-box))
(defun get-surface-render-box (surface output view-x view-y surface-x surface-y)
  (declare (type cffi:foreign-pointer surface))
  (multiple-value-bind (output-x output-y)
      (wlr:output-layout-coords (output-layout (get-output-manager (get-server))) output)
    (setf output-x (+ output-x view-x surface-x)
	  output-y (+ output-y view-y surface-y))
    (with-wlr-accessors ((scale :scale))
	output (:struct wlr:output)
      (with-wlr-accessors ((current :current))
	  surface (:struct wlr:surface)
	(make-instance 'wlr:box
		       :x (truncate (* scale output-x))
		       :y (truncate (* scale output-y))
		       :width (truncate (* (getf current :width) scale))
		       :height (truncate (* (getf current :height) scale)))))))

(defcallback draw-surface :void
    ((surface :pointer)
     (sx :int)
     (sy :int)
     (render-data :pointer))
  (with-foreign-slots ((output renderer time view-x view-y) render-data (:struct render-data))
    (let ((texture (wlr:surface-get-texture surface)))
      (when (null-pointer-p texture)
	(return-from draw-surface))
      (with-wlr-accessors ((current :current))
	  surface (:struct wlr:surface)
	(let ((box (get-surface-render-box surface output view-x view-y sx sy))
	      (transform (wlr:output-transform-invert (getf current :transform))))
	  (with-foreign-object (matrix :float 9)
	    (with-wlr-accessors ((transform-matrix :transform-matrix :pointer t))
		output (:struct wlr:output)
	      (wlr:matrix-project-box matrix box
				      transform
				      0.0 transform-matrix)
	      (wlr:render-texture-with-matrix renderer texture matrix 1.0)
	      (wlr:surface-send-frame-done surface time))))))))

(defun draw-frame (wlr-output output)
  (declare (type mahogany-output output))
  (let ((wlr-renderer (wlr:backend-get-renderer (foreign-slot-value (output-wlr-output output)
						       '(:struct wlr:output)
						       :backend))))
    (wlr:output-make-current wlr-output (cffi:null-pointer))
    (wlr:renderer-begin wlr-renderer (output-width output) (output-height output))
    (wlr:renderer-clear wlr-renderer #(0.4 0.4 0.4 1.0))

    (with-foreign-objects ((data '(:struct render-data))
			   (now  '(:struct timespec)))
      (clock-get-time :monotonic now)
      (with-foreign-slots ((output renderer time view-x view-y) data (:struct render-data))
	(setf output wlr-output
	      renderer wlr-renderer
	      time now)
	(dolist (view (get-visible-views (server-frontend (get-server))))
	  (when (view-mapped view)
	    (setf view-x (view-x view)
		  view-y (view-y view))
            (view-for-each-surface view (callback draw-surface) data)))))

    (wlr:renderer-end wlr-renderer)
    (wlr:output-swap-buffers wlr-output (cffi:null-pointer)
			     (cffi:null-pointer))))

(cffi:defcallback new-frame-notify :void
    ((listener :pointer)
     (output :pointer))
  (let ((output-owner (get-listener-owner listener *listener-hash*)))
    (draw-frame output output-owner)))

(defun make-mahogany-output (output)
  (let ((frame-listener (make-listener new-frame-notify)))
    (assert (not (cffi:null-pointer-p frame-listener)))
    (with-wlr-accessors ((frame-event :event-frame :pointer t)
			 (modes :modes :pointer t))
	output (:struct wlr:output)
      (wayland-server-core:wl-signal-add frame-event frame-listener)
      (when (not (eq 1 (wl-list-empty modes)))
	;; TODO: allow other default modes:
	(let ((mode (container-of (foreign-slot-value modes
							'(:struct wl_list)
							'prev)
				  '(:struct wlr:output-mode)
				  :link)))
	  (print-output-mode mode)
	  (wlr:output-set-mode output mode))))

    (let ((new-output (make-instance 'mahogany-output
				     :wlr-output output
    				     :frame-listener frame-listener)))
      (register-listener frame-listener new-output *listener-hash*)
      (the mahogany-output new-output))))

(defun destroy-mahogany-output (mahogany-output)
  (unregister-listener (output-frame-listener mahogany-output) *listener-hash*)
  (wl-list-remove (cffi:foreign-slot-pointer (output-frame-listener mahogany-output)
    					     '(:struct wl_listener) 'link))
  (foreign-free (output-frame-listener mahogany-output)))
