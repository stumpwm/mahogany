(defpackage #:mahogany/backend/server
  (:use :cl :mahogany/backend/output-manager :mahogany/log
	:mahogany/backend/server-protocol)
  (:import-from :mahogany/backend/input/input-manager
		:make-input-manager)
  (:import-from :cffi
		:null-pointer)
  (:import-from :wayland-server-core
		:wl-display-destroy
		:wl-display-destroy-clients
		:wl-display-run))

(in-package #:mahogany/backend/server)

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

(defmethod initialize-instance :after ((server server) &key)
  ;; initialize everything, no initargs
  (with-accessors ((backend get-backend) (display get-display)
		   (output-manager get-output-manager)
		   (input-manager get-input-manager)
		   (renderer get-renderer)
		   (data-device-manager get-data-device-manager))
      server
    ;; TODO: actual error handling here
    (setf display (wayland-server-core:wl-display-create))
/    (assert (not (eql (null-pointer) display)))
    (setf backend (wlr:backend-autocreate display (null-pointer)))
    (assert (not (and backend
		      (eql (null-pointer) backend))))
    (setf renderer (wlr:backend-get-renderer backend))
    (assert (not (eql renderer (null-pointer))))
    (wlr:renderer-init-wl-display renderer display)
    (setf output-manager (make-output-manager backend))
    (setf input-manager (make-input-manager server))))

(defun make-server ()
  (make-instance 'server))

(defun destroy-server (server)
  ;; free the stuff in the output-manager first:
  (destroy-output-manager (get-output-manager server))
  (wl-display-destroy-clients (get-display server))
  (wl-display-destroy (get-display server)))

(defun run-server ()
  (wlr:log-init :log-debug (cffi:null-pointer))
  (log-init :level :trace)
  (setf *server* (make-server))
  (unless (wlr:backend-start (get-backend *server*))
    (format t "Could not start backend")
    (wl-display-destroy (get-display *server*))
    (uiop:quit 1))
  (wl-display-run (get-display *server*))
  (destroy-server *server*)
  (uiop:quit))
