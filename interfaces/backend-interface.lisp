(in-package #:mahogany/backend-interface)

(defgeneric set-window-manager (backend wm)
  (:documentation "Set the backend's window manager"))

(defgeneric start-backend (backend &key &allow-other-keys)
  (:documentation "Start running the IO loop."))

(defgeneric stop-backend (backend)
  (:documentation "Stop the IO loop associated with the backend"))

(defgeneric cleanup-backend (backend)
  (:documentation "Cleanup the resources associated with the backend"))

;; output functions

(defgeneric output-width (output)
  (:documentation "Get the width of the output"))

(defgeneric output-height (output)
  (:documentation "Get the width of the output"))
