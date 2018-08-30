(defpackage #:mh/backend/server
  (:use :cl :mh/backend/desktop :mh-log)
  (:import-from :cffi
		:null-pointer)
  (:import-from :wayland-server-core
		:wl-display-destroy
		:wl-display-destroy-clients
		:wl-display-run))

(in-package #:mh/backend/server)

(export '(get-server
	  clear-server
	  run-server))

(defvar *server* nil)

(defun get-server ()
  (if *server*
      *server*
      (setf *server* (make-server))))

(defun clear-server ()
  (when *server*
    (destroy-server *server*)
    (setf *server* nil)))

(defclass server ()
  ;; should probably use :reader instead of :accessor
  ((display :accessor display)
   (backend :accessor backend)
   (desktop :accessor desktop)
   (input :accessor input)
   (renderer :accessor renderer)
   (data-device-manager :accessor data-device-manager))
  (:documentation "Class used to store and manage the wayland backend for Mahogany"))

(defmethod initialize-instance :after ((server server) &key)
  ;; initialize everything, no initargs
  (with-accessors ((backend backend) (display display)
		   (desktop desktop) (renderer renderer) (data-device-manager data-device-manager))
      server
    ;; TODO: actual error handling here
    (setf display (wayland-server-core:wl-display-create))
    (assert (not (eql (null-pointer) display)))
    (setf backend (wlr:backend-autocreate display (null-pointer)))
    (assert (not (and backend
		      (eql (null-pointer) backend))))
    (setf renderer (wlr:backend-get-renderer backend))
    (assert (not (eql renderer (null-pointer))))
    (wlr:renderer-init-wl-display renderer display)
    (setf desktop (make-desktop backend))))
;; (setf input (make-input backend)

(defun make-server ()
  (make-instance 'server))

(defun destroy-server (server)
  ;; free the stuff in the desktop first:
  (destroy-desktop (desktop server))
  (wl-display-destroy-clients (display server))
  (wl-display-destroy (display server)))

(defun run-server ()
  (cl-wlroots/util/log:log-init :log-debug (cffi:null-pointer))
  (setf *server* (make-server))
  (unless (wlr:backend-start (backend *server*))
    (format t "Could not start backend")
    (wl-display-destroy (display *server*))
    (uiop:quit 1))
  (wl-display-run (display *server*))
  (destroy-server *server*)
  (uiop:quit))
