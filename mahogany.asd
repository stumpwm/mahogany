;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:bordeaux-threads
		      #:cl-wlroots #:cffi #:cl-ansi-text #:terminfo
		      #:cl-ppcre #:vom #:cl-egl)
  :components ((:module "src/"
			:serial t
			:components
			((:file "util")
			 (:file "log")))
	       (:module "src/backend"
			:serial t
			:components
			((:file "util")
			 (:file "output")
			 (:file "desktop")
			 (:file "server")
			 (:file "test")
			 (:file "backend")
			 (:file "package")))
	       (:module "src/base"
			:serial t
			:components
			((:file "package")
			 (:file "mahogany"))))
  :in-order-to ((test-op (test-op mahogany-test))))
