(in-package #:mahogany/backend)

(defvar *server* nil)

(defun get-server ()
  (if *server*
      *server*
      (setf *server* (make-instance 'server))))

(defgeneric get-display (server)
  (:documentation "Get the wlroots display from the object"))

(defgeneric get-backend (server)
  (:documentation "Get the wlroots backend from the object"))

(defgeneric get-output-manager (server)
  (:documentation "Get the mahogany output manager from the object"))

(defgeneric get-input-manager (server)
  (:documentation "Get the mahogany input manager from the object"))

(defgeneric stop-server (server)
  (:documentation "Stop the server and exit its event loop."))

(defclass server ()
  ;; should probably use :reader instead of :accessor
  ((display :accessor get-display)
   (backend :accessor get-backend)
   (output-manager :accessor get-output-manager)
   (input-manager :accessor get-input-manager)
   (renderer :accessor get-renderer)
   (data-device-manager :accessor get-data-device-manager))
  (:documentation "Class used to store and manage the wayland backend for Mahogany"))
