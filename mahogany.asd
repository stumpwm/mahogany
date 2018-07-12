;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-autowrap #:uiop #:alexandria #:cl-ppcre #:bordeaux-threads)
  :components ((:module "base"
			:serial t
			:components
			((:file "package")
			 (:file "mahogany"))))
  :in-order-to ((test-op (test-op mahogany-test))))
