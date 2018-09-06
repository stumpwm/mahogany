;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :serial t
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:bordeaux-threads
		      #:cl-wlroots #:cffi #:cl-ansi-text #:terminfo
		      #:cl-ppcre #:vom #:cl-egl #:mahogany/all)
  :in-order-to ((test-op (test-op mahogany-test))))

(asdf:register-system-packages "cl-wayland" '(:wayland-server-core))
