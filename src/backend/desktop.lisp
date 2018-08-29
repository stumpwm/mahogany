(defpackage #:mh/backend/desktop
  (:use #:cl #:autowrap #:wlr/wayland #:plus-c)
  (:import-from #:mh-util
		#:free-from)
  (:import-from #:mh-log
		#:log-string))

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
  (let ((output-listener (wlr/wayland:make-listener 'handle-new-output))
	(layout (c-fun wlr:wlr-output-layout-create)))
    (wlr/wayland:wl-signal-add (c-ref backend (:struct (wlr:wlr-backend)) :events :new-output &)
			       ;;(c-ref output-stream-p (:sttruct (wlr:wlr-listener)) :notify
			       output-listener)
    (make-instance 'desktop
		   :output-listener output-listener
		   :layout layout)))

(defun destroy-desktop (desktop)
  ;; remove the listener from its signal:
  (c-fun wlr:wl-list-remove (c-ref (output-listener desktop) (:struct (wlr:wl-listener)) :link &))
  (free-from desktop 'output-listener)
  (c-fun wlr:wlr-output-layout-destroy (layout desktop)))
