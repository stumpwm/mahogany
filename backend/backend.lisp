;; (defclass backend-callbacks ())

;; (defclass client

(defgeneric init-backend (backend callbacks)
  (:documentation "Initialize the backend using the functions specified in callbacks
to handle events"))

(defgeneric start-backend (backend)
  (:documentation "Start the compositor/window manager"))
