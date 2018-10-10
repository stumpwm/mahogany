(in-package :mahogany/backend)

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
  (declare (ignore listener something))
  ;; TODO: do layout change
  (log-string :info "Output layout changed"))

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
    (cffi:foreign-free listener)))

(cffi:defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (let ((manager (get-listener-owner listener *listener-hash*))
	(destroy-listener (make-listener destroy-output)))

    (log-string :info "New output ~A" (foreign-string-to-lisp (foreign-slot-pointer output '(:struct wlr:output)
  							       :name)))
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
    (unregister-listener output-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer output-listener
					  '(:struct wl_listener) 'link))
    (foreign-free output-listener)
    (unregister-listener layout-change-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer layout-change-listener
					  '(:struct wl_listener) 'link))
    (wlr:output-layout-destroy (output-layout output-manager))
    (foreign-free layout-change-listener)))
