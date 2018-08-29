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
  ((layout :initarg :layout
	   :accessor layout)
   (output-listener :initarg :output-listener
		    :accessor output-listener)
   (compositor :initarg :compositor
	       :accessor :compositor)
   (outputs :accessor desktop-outputs
	    :type 'list
	    :initform ())))

(defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (let ((desktop-owner (get-listener-owner listener *desktop-listeners*)))
    (assert (not (null desktop-owner)))
    (log-string :debug "New output: ~A" (cffi:foreign-string-to-lisp
					 (foreign-slot-pointer output 'wlr:output
							       :name)))
    (finish-output)
    (let* ((destroy-listener (make-listener handle-destroy-output))
	   (new-output (make-output output)))
      ;; register the signal:
      (wl-signal-add (cffi:foreign-slot-pointer output '(:struct wlr:output) :event-destroy)
		     destroy-listener)
      ;; add the output to the desktop:
      (push new-output (desktop-outputs desktop-owner))
      (wlr:output-layout-add-auto (layout desktop-owner) output)
      (register-listener destroy-listener (cons desktop-owner new-output) *desktop-listeners*))))

(defcallback handle-destroy-output :void
    ((listener :pointer)
     (removed-wlr-output :pointer))
  (log-string :debug "Removed output: ~A" (cffi:foreign-string-to-lisp
					     (foreign-slot-pointer removed-wlr-output
								   '(:struct wlr:output)
								   :name)))
  (let* ((owner-cons (get-listener-owner listener *desktop-listeners*))
	 (desktop-owner (car owner-cons))
	 (output (cdr owner-cons)))
    (unregister-listener listener *desktop-listeners*)
    (wlr:output-layout-remove (layout desktop-owner) (output-output output))
    (remove output (desktop-outputs desktop-owner))
    (wl-list-remove (cffi:foreign-slot-pointer listener
					  '(:struct wl_listener) 'link))
    (destroy-output output)
    (cffi:foreign-free listener)))

(defun make-desktop (backend)
  (let ((new-output-listener (make-listener handle-new-output))
	(layout (wlr:output-layout-create)))
    (wl-signal-add (cffi:foreign-slot-pointer backend '(:struct wlr:backend) :event-new-output)
		   new-output-listener)
    (let ((new-desktop (make-instance 'desktop
			 :output-listener new-output-listener
			 :layout layout)))
      (backend/util:register-listener new-output-listener new-desktop *desktop-listeners*)
      (the desktop new-desktop))))

(defun destroy-desktop (desktop)
  ;; remove the listener from its signal:
  (wl-list-remove (cffi:foreign-slot-pointer (output-listener desktop)
					'(:struct wl_listener) link))
  (free-from desktop 'output-listener)
  (wlr:output-layout-destroy (layout desktop)))
