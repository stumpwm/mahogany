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

(defclass desktop ()
  ((layout :initarg :layout
	   :accessor layout)
   (output-listener :initarg :output-listener
		    :accessor output-listener)
   (outputs :accessor outputs
	    :type 'list
	    :initform ())))

(defcallback handle-new-output :void
    ((listener :pointer)
     (data :pointer))
  (declare (ignore listener data))
  (print "New output")
  (finish-output))

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
