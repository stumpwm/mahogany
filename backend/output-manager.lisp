(in-package :mahogany/backend)

(defun print-output-mode (mode)
  (with-wlr-accessors ((width :width)
  		       (height :height)
  		       (flags :flags)
  		       (refresh :refresh))
      mode (:struct wlr:output-mode)
    (log-string :trace "~Sx~S @ ~S flags: ~S" width height refresh flags)))

(defclass output-manager ()
  ((output-listener :initarg :output-listener
		    :reader output-listener)
   (outputs :accessor get-outputs
	    :type 'list
	    :initform ())
   (layout :initarg :output-layout
	   :accessor output-layout
	   :type wlr:output-layout)
   (layout-change-listener :initarg :layout-change-listener
			   :reader output-layout-listener
			   :type wl_listener)))

(cffi:defcallback handle-layout-change :void
    ((listener :pointer)
     (something :pointer))
  (declare (ignore something))
  (let* ((manager (get-listener-owner listener *listener-hash*))
	(layout (output-layout manager)))
    (dolist (output (get-outputs manager))
      (let (x-coord y-coord)
	(with-wlr-accessors ((x :x) (y :y))
	    (wlr:output-layout-get layout (output-wlr-output output))
	    (:struct wlr:output-layout-output)
	  (setf (output-x output) x
		(output-y output) y))
	(log-string :info "Layout changed: ~A (~S ~S) :w ~S :h ~S"
		    (foreign-string-to-lisp (foreign-slot-pointer (output-wlr-output output)
								  '(:struct wlr:output)
  								  :name))
		    (output-x output) (output-y output)
		    (output-width output)
		    (output-height output))))))

(cffi:defcallback destroy-output :void
    ((listener :pointer)
     (output :pointer))
  (log-string :info "Output destroyed: ~A" (foreign-string-to-lisp
					      (foreign-slot-pointer output '(:struct wlr:output)
  								    :name)))
  (multiple-value-bind (manager lookup-output)
      (values-list (get-listener-owner listener *listener-hash*))
    (destroy-mahogany-output lookup-output)
    (setf (get-outputs manager) (delete lookup-output (get-outputs manager)))
    (unregister-listener listener *listener-hash*)
    (wl-list-remove (cffi:foreign-slot-pointer listener
    					       '(:struct wl_listener) 'link))
    (wlr:output-layout-remove (output-layout manager) output)
    (cffi:foreign-free listener)))

(cffi:defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (let ((manager (get-listener-owner listener *listener-hash*))
	(destroy-listener (make-listener destroy-output)))
    (log-string :info "New output ~A :w ~S :h ~S"
		(foreign-string-to-lisp (foreign-slot-pointer output '(:struct wlr:output)
  							      :name))
		(foreign-slot-value output
		      '(:struct wlr:output)
		      :width)
		(foreign-slot-value output
				    '(:struct wlr:output)
				    :height))

    (assert (not (cffi:null-pointer-p destroy-listener)))
    (assert (not (null manager)))

    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-destroy)
    				       destroy-listener)
    (wlr:output-layout-add-auto (output-layout manager) output)
    (let ((new-output (make-mahogany-output output)))
      ;; insert both the manager and the output so we don't have to look it up later:
      (register-listener destroy-listener (list manager new-output) *listener-hash*)
      (push new-output (get-outputs manager))
      (config-pointers-for-output (get-input-manager (get-server)) new-output))))

(defun make-output-manager (backend)
  (let ((new-output-listener (make-listener handle-new-output))
	(layout-change-listener (make-listener handle-layout-change))
	(layout (wlr:output-layout-create)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-output)
				       new-output-listener)
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer layout
								  '(:struct wlr:output-layout)
								  :event-change)
				       layout-change-listener)
    (let ((new-manager (make-instance 'output-manager
				      :output-layout layout
				      :layout-change-listener layout-change-listener
				      :output-listener new-output-listener)))
      (register-listener layout-change-listener new-manager *listener-hash*)
      (register-listener new-output-listener new-manager *listener-hash*)
      (the output-manager new-manager))))

(defun destroy-output-manager (output-manager)
  (with-accessors ((output-listener output-listener)
		   (layout-change-listener output-layout-listener))
      output-manager
    (log-string :debug "Output manager destroyed")
    (unregister-listener output-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer output-listener
					  '(:struct wl_listener) 'link))
    (foreign-free output-listener)
    (unregister-listener layout-change-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer layout-change-listener
					  '(:struct wl_listener) 'link))
    (wlr:output-layout-destroy (output-layout output-manager))
    (foreign-free layout-change-listener)))
