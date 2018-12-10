(in-package #:mahogany/backend)

(export '(run-server))

(defvar *server* nil)

;; internal functions:

(defgeneric get-display (server)
  (:documentation "Get the wlroots display from the object"))

(defgeneric get-backend (server)
  (:documentation "Get the wlroots backend from the object"))

(defgeneric get-output-manager (server)
  (:documentation "Get the mahogany output manager from the object"))

(defgeneric get-input-manager (server)
  (:documentation "Get the mahogany input manager from the object"))

(defgeneric get-client-manager (server)
  (:documentation "Get the client manager from this ohject"))

(defclass server ()
  ;; should probably use :reader instead of :accessor
  ((display :accessor get-display)
   (backend :accessor get-backend)
   (wlr-compositor :accessor server-compositor)
   (output-manager :accessor get-output-manager)
   (input-manager :accessor get-input-manager)
   (client-manager :accessor get-client-manager)
   (renderer :accessor get-renderer)
   (data-device-manager :accessor get-data-device-manager))
  (:documentation "Class used to store and manage the wayland backend for Mahogany"))

(defmethod initialize-instance :after ((server server) &key)
  ;; initialize everything, no initargs
  (with-accessors ((backend get-backend) (display get-display)
		   (output-manager get-output-manager)
		   (input-manager get-input-manager)
		   (client-manager get-client-manager)
		   (renderer get-renderer)
		   (compositor server-compositor)
		   (data-device-manager get-data-device-manager))
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

    ;; setup freebee interfaces:
    (setf compositor (wlr:compositor-create display renderer))
    (setf data-device-manager (wlr:data-device-manager-create display))

    (setf output-manager (make-output-manager backend))
    (setf input-manager (make-input-manager server))
    (setf client-manager (make-client-manager display))))

(defun make-server ()
  (make-instance 'server))

(defun destroy-server (server)
  ;; cleanup the wl resources first:
  (wl-display-destroy-clients (get-display server))
  (wl-display-destroy (get-display server))
  ;; destroy the resources we created:
  (destroy-output-manager (get-output-manager server))
  (destroy-input-manager (get-input-manager server))
  (destroy-client-manager (get-client-manager server)))

(defmethod stop-backend ((server server))
  (wl-display-terminate (get-display *server*)))

(defmethod cleanup-backend ((server server))
  (clear-server))

(defmethod start-backend ((server server) &key &allow-other-keys)
  (let ((socket (wl-display-add-socket-auto (get-display server))))
    (unless socket
      (log-string :fatal "Could not create Wayland socket")
      (wl-display-destroy (get-display server))
      (uiop:quit 1))
    (unless (wlr:backend-start (get-backend server))
      (log-string :fatal "Could not start backend")
      (wl-display-destroy (get-display server))
      (uiop:quit 1))
    (setf (uiop:getenv "WAYLAND_DISPLAY") socket)
    (log-string :info "Starting server on wayland display ~A" socket)
    (wl-display-run (get-display server))))

(defun get-server ()
  (if *server*
      *server*
      (setf *server* (make-instance 'server))))

(defun clear-server ()
  (when *server*
    (destroy-server *server*)
    (setf *server* nil)))

(defun run-server (&optional backend-type)
  (wlr:log-init :log-debug (cffi:null-pointer))
  (log-init :level :trace)
  (let ((server (get-server)))
    (start-backend server)
    (clear-server)))
