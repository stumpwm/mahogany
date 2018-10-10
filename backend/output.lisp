(in-package #:mahogany/backend)

(defclass mahogany-output ()
  ((wlr-output :initarg :wlr-output
	      :reader output-wlr-output
	      :type wlr:output)
  (frame-listener :initarg :frame-listener
		  :reader output-frame-listener
		  :type wl_listener)))

(cffi:defcallback new-frame-notify :void
    ((listener :pointer)
     (output :pointer))
  (declare (ignore output))
  (let* ((output-owner (get-listener-owner listener *listener-hash*))
	 (renderer (wlr:backend-get-renderer (foreign-slot-value (output-wlr-output output-owner)
						       '(:struct wlr:output)
						       :backend))))
    (wlr:output-make-current (output-wlr-output output-owner) (cffi:null-pointer))

    (wlr:renderer-clear renderer #(0.4 0.4 0.4 1.0))
    (wlr:output-swap-buffers (output-wlr-output output-owner) (cffi:null-pointer)
			     (cffi:null-pointer))
    (wlr:renderer-end renderer)))

(defun make-mahogany-output (output)
  (let ((frame-listener (make-listener new-frame-notify)))
    (assert (not (cffi:null-pointer-p frame-listener)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-frame)
    				       frame-listener)
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
