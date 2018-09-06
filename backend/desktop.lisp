(defpackage #:mahogany/backend/desktop
  (:use #:cl #:cffi #:mahogany/backend/output #:mahogany/backend/util)
  (:import-from :mahogany/log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link))

(in-package #:mahogany/backend/desktop)

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
		    :reader output-listener)
   (outputs :accessor desktop-outputs
	    :type 'list
	    :initform ())))

(defvar *listener-hash* (make-hash-table))

(cffi:defcallback destroy-output :void
    ((listener :pointer)
     (output :pointer))
  (log-string :info "Output destroyed: ~A" (foreign-string-to-lisp
					      (foreign-slot-pointer output '(:struct wlr:output)
  								    :name)))
  (multiple-value-bind (desktop-owner lookup-output)
      (values-list (get-listener-owner listener *listener-hash*))
    (destroy-mahogany-output lookup-output)
    (delete lookup-output (desktop-outputs desktop-owner))
    (unregister-listener listener *listener-hash*)
    (wl-list-remove (cffi:foreign-slot-pointer listener
    					       '(:struct wl_listener) 'link))
    (cffi:foreign-free listener)))

(cffi:defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (declare (ignore listener))
  (let ((desktop-owner (get-listener-owner listener *listener-hash*))
	(destroy-listener (make-listener destroy-output)))

    (log-string :info "New output ~A" (foreign-string-to-lisp (foreign-slot-pointer output '(:struct wlr:output)
  							       :name)))
    (assert (not (cffi:null-pointer-p destroy-listener)))
    (assert (not (null desktop-owner)))

    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-destroy)
    				       destroy-listener)
    (let ((new-output (make-mahogany-output output)))
      ;; insert both the desktop and the output so we don't have to look it up later:
      (register-listener destroy-listener (list desktop-owner new-output) *listener-hash*)
      (push new-output (desktop-outputs desktop-owner)))))

(defun make-desktop (backend)
  (let ((new-output-listener (make-listener handle-new-output)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-output)
				       new-output-listener)
    (let ((new-desktop (make-instance 'desktop
			 :output-listener new-output-listener)))
      (register-listener new-output-listener new-desktop *listener-hash*)
      (the desktop new-desktop))))
