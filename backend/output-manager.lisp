(defpackage :mahogany/backend/output-manager
  (:use #:cl #:cffi #:mahogany/backend/output #:mahogany/backend/util)
  (:import-from :mahogany/log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link))

(in-package :mahogany/backend/output-manager)

(export '(output-manager
	  make-output-manager
	  destroy-output-manager))

(defclass output-manager ()
  ((output-listener :initarg :output-listener
		    :reader output-listener)
   (outputs :accessor get-outputs
	    :type 'list
	    :initform ())))

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
    (let ((new-output (make-mahogany-output output)))
      ;; insert both the manager and the output so we don't have to look it up later:
      (register-listener destroy-listener (list manager new-output) *listener-hash*)
      (push new-output (get-outputs manager)))))

(defun make-output-manager (backend)
  (let ((new-output-listener (make-listener handle-new-output)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-output)
				       new-output-listener)
    (let ((new-manager (make-instance 'output-manager
			 :output-listener new-output-listener)))
      (register-listener new-output-listener new-manager *listener-hash*)
      (the output-manager new-manager))))

(defun destroy-output-manager (output-manager)
  (with-accessors ((listener output-listener)) output-manager
    (unregister-listener listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer listener
					  '(:struct wl_listener) 'link))
    (foreign-free listener)))
