(defpackage #:mh/backend/server
  (:use :cl :mh/backend/desktop)
  (:import-from :cffi
		:null-pointer)
  (:import-from :plus-c
		:c-fun))

(in-package #:mh/backend/server)

(export '(make-server
	  destroy-server))

(defclass server ()
  ;; should probably use :reader instead of :accessor
  ((display :accessor display)
   (backend :accessor backend)
   (event-loop :accessor event-loop)
   (desktop :accessor desktop)
   (input :accessor input)
   (renderer :accessor renderer)
   (data-device-manager :accessor data-device-manager))
  (:documentation "Class used to store and manage the wayland backend for Mahogany"))

(defmethod initialize-instance :after ((server server) &key)
  ;; initialize everything, no initargs
  (with-accessors ((event-loop event-loop) (backend backend) (display display)
		   (desktop desktop) (renderer renderer) (data-device-manager data-device-manager))
      server
    ;; TODO: actual error handling here
    (setf display (c-fun wlr:wl-display-create))
    (assert (not (eql (null-pointer) display)))
    (setf event-loop (c-fun wlr:wl-display-get-event-loop display))
    (setf backend (c-fun wlr:wlr-backend-autocreate display (null-pointer)))
    (assert (not (and (eql (null-pointer) event-loop)
		      (eql (null-pointer) backend))))
    ;;(setf renderer (c-fun wlr:wlr-backend-get-renderer backend))
    ;; (c-fun wlr:wlr-renderer-init renderer)
    ;; (setf data-device-manager (c-fun wlr:wlr-data-device-manager-create display))
    (setf desktop (make-desktop backend))))
;; (setf input (make-input backend)

(defun make-server ()
  (make-instance 'server))

(defun destroy-server (server)
  ;; free the stuff in the desktop first:
  ;;(destroy-desktop (desktop server))
  (c-fun wlr:wl-display-destroy-clients (display server))
  (c-fun wlr:wl-display-destroy (display server)))

(defun run-server ()
  (c-fun wlr:wlr-log-init 3 (cffi:null-pointer))
  (setf *server* (make-server))
  (unless (c-fun wlr:wlr-backend-start (backend *server*))
    (format t "Could not start backend")
    (c-fun wlr:wl-display-destroy (display *server*))
    (uiop:quit 1))
  (c-fun wlr:wl-display-run (display *server*))
  (destroy-server *server*)
  (uiop:quit))
