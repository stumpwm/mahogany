;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

#+sbcl
(sb-int:set-floating-point-modes :traps '(:OVERFLOW))

(defpackage #:cl-wlr-test
  (:use #:plus-c #:autowrap #:mh-util #:cl #:wlr/wayland))

(in-package #:cl-wlr-test)

(export '(main))

(defclass desktop ()
  ((layout :initarg :layout
	   :accessor layout)
   (output-listener :initarg :output-listener
		    :accessor output-listener)
   (outputs :accessor outputs
	    :initform ())))

(defclass input-manager ()
  ((input-listener :initarg :input-listener)
   (seats :initform () :accessor :seats)))

(autowrap:defcallback handle-new-output :void
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

(defclass test-server ()
  ((display :initarg :display
	    :accessor display)
   (backend :initarg :backend
	    :accessor backend)
   (event-loop :initarg :event-loop
	       :accessor event-loop)
   (input-listener :initarg :input-listener
		   :accessor input-listener)
   (desktop :accessor desktop
	    :initform nil)))

(defmethod initialize-instance :after ((server test-server) &key)
  (setf (desktop server) (make-desktop (backend server))))


(defun make-server ()
  (let ((display (plus-c:c-fun wlr:wl-display-create)))
    (let ((event-loop (c-fun wlr:wl-display-get-event-loop display))
	  (backend (c-fun wlr:wlr-backend-autocreate display (cffi:null-pointer))))
      ;;(input-listener (
      ;; (plus-c:c-fun
      (make-instance 'test-server
		     :display display
		     :backend backend
		     :event-loop event-loop))))

(defun destroy-server (server)
  ;; free the stuff in the desktop first:
  ;;(destroy-desktop (desktop server))
  (c-fun wlr:wl-display-destroy-clients (display server))
  (c-fun wlr:wl-display-destroy (display server)))

(defun main ()
  (c-fun wlr:wlr-log-init 3 (cffi:null-pointer))
  (setf *server* (make-server))
  (unless (c-fun wlr:wlr-backend-start (backend *server*))
    (format t "Could not start backend")
    (c-fun wlr:wl-display-destroy (display *server*))
    (uiop:quit 1))
  (c-fun wlr:wl-display-run (display *server*))
  (destroy-server *server*)
  (uiop:quit))
