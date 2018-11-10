;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:bordeaux-threads
		      #:cl-wlroots #:cffi #:cl-ansi-text #:terminfo
		      #:cl-ppcre #:cl-egl)
  :in-order-to ((test-op (test-op mahogany-test)))
  :components ((:file "package")
	       (:file "log")
	       (:module backend
			:components ((:file "util")
				     (:file "output")
				     (:file "output-manager")
				     (:file "client-manager")
				     (:file "input/input-devices")
				     (:file "input/keyboard")
				     (:file "input/pointing-device")
				     (:file "input/seat")
				     (:file "input/input-manager")
				     (:file "server")))))
