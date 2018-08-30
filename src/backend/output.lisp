(defpackage #:mh/backend/output
  (:use :cl :cffi :wayland-server-core :backend/util)
  (:import-from :mh-log
		:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:link))

(in-package #:mh/backend/output)

(export '(make-mh-output
	  destroy-mh-output))

(defvar *listener-hash* (make-hash-table))

(defclass mh-output ()
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
	 ;; (renderer (wlr:backend-get-renderer (sample-state-backend *sample-state*)))
	 (renderer (wlr:backend-get-renderer (foreign-slot-value (output-wlr-output output-owner)
						       '(:struct wlr:output)
						       :backend))))
    (wlr:output-make-current (output-wlr-output output-owner) (cffi:null-pointer))

    (with-foreign-array (color #(0.4 0.4 0.4 1.0) '(:array :float 4))
      (wlr:renderer-clear renderer color))
    (wlr:output-swap-buffers (output-wlr-output output-owner) (cffi:null-pointer)
			     (cffi:null-pointer))
    (wlr:renderer-end renderer)))

(defun make-mh-output (output)
  (let ((frame-listener (make-listener new-frame-notify)))
    (assert (not (cffi:null-pointer-p frame-listener)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-frame)
    				       frame-listener)
    (let ((new-output (make-instance 'mh-output
				     :wlr-output output
    				     :frame-listener frame-listener)))
      (register-listener frame-listener new-output *listener-hash*)
      (the mh-output new-output))))

(defun destroy-mh-output (mh-output)
  (unregister-listener (output-frame-listener mh-output) *listener-hash*)
  (wl-list-remove (cffi:foreign-slot-pointer (output-frame-listener mh-output)
    					     '(:struct wl_listener) 'link))
  (foreign-free (output-frame-listener mh-output)))
