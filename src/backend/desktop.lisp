(defpackage #:mh/backend/desktop
  (:use #:cl #:cffi #:mh/backend/output #:backend/util)
  (:import-from :mh-log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link))

(in-package #:mh/backend/desktop)

(export '(desktop
	  make-desktop
	  destroy-desktop))

;; used to match listeners with their desktops
;; there's supposted to only be one desktop, so...
;; could also use a list of desktops, and see if the listener
;; is a member of that desktop...

(defvar *desktop-listeners* (make-hash-table))

(defclass desktop ()
  ((output-listener :initarg :output-listener
		    :accessor output-listener)
   (outputs :accessor desktop-outputs
	    :type 'list
	    :initform ())))

(defstruct sample-output
  output
  frame-listener
  destroy-listener)

(defvar *listener-hash* (make-hash-table))

(cffi:defcallback new-frame-notify :void
    ((listener :pointer)
     (output :pointer))
  (declare (ignore output))
  (let* ((output-owner (get-listener-owner listener *listener-hash*))
	 ;; (renderer (wlr:backend-get-renderer (sample-state-backend *sample-state*)))
	 (renderer (wlr:backend-get-renderer (foreign-slot-value (sample-output-output output-owner)
						       '(:struct wlr:output)
						       :backend))))
    (wlr:output-make-current (sample-output-output output-owner) (cffi:null-pointer))

    (with-foreign-array (color #(0.4 0.4 0.4 1.0) '(:array :float 4))
      (wlr:renderer-clear renderer color))
    (wlr:output-swap-buffers (sample-output-output output-owner) (cffi:null-pointer)
			     (cffi:null-pointer))
    (wlr:renderer-end renderer)))

(cffi:defcallback destroy-output :void
    ((listener :pointer)
     (output :pointer))
  (log-string :info "Output destroyed: ~A" (foreign-string-to-lisp
					      (foreign-slot-pointer output '(:struct wlr:output)
  								    :name)))
  (multiple-value-bind (desktop-owner lookup-output)
      (values-list (get-listener-owner listener *listener-hash*))
    (assert (eql (pointer-address output)
		 (pointer-address (sample-output-output lookup-output))))
    (delete lookup-output (desktop-outputs desktop-owner))
    (unregister-listener listener *listener-hash*)
    (unregister-listener (sample-output-frame-listener lookup-output) *listener-hash*)
    (wl-list-remove (cffi:foreign-slot-pointer listener
    					       '(:struct wl_listener) 'link))
    (wl-list-remove (cffi:foreign-slot-pointer (sample-output-frame-listener lookup-output)
    					       '(:struct wl_listener) 'link))
    (cffi:foreign-free listener)
    (cffi:foreign-free (sample-output-frame-listener lookup-output))))

(cffi:defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (declare (ignore listener))
  (assert (not (cffi:null-pointer-p output)))
  (let ((desktop-owner (get-listener-owner listener *listener-hash*))
	(frame-listener (make-listener new-frame-notify))
	(destroy-listener (make-listener destroy-output)))

    (log-string :info "New output ~A" (foreign-string-to-lisp (foreign-slot-pointer output '(:struct wlr:output)
  							       :name)))
    (assert (not (cffi:null-pointer-p frame-listener)))
    (assert (not (cffi:null-pointer-p destroy-listener)))
    (assert (not (null desktop-owner)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-frame)
    				       frame-listener)
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-destroy)
    				       destroy-listener)
    (let ((new-output (make-sample-output :output output
    					  :frame-listener frame-listener
    					  :destroy-listener destroy-listener)))
      ;; insert both the desktop and the output so we don't have to look it up later:
      (register-listener destroy-listener (list desktop-owner new-output) *listener-hash*)
      (register-listener frame-listener new-output *listener-hash*)
      (push new-output (desktop-outputs desktop-owner)))))

(defun make-desktop (backend)
  (log-string :debug "Backend in desktop: ~S" backend)
  (let ((new-output-listener (make-listener handle-new-output)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-output)
				       new-output-listener)
    (let ((new-desktop (make-instance 'desktop
			 :output-listener new-output-listener)))
      (backend/util:register-listener new-output-listener new-desktop *listener-hash*)
      (the desktop new-desktop))))
