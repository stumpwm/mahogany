;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :depends-on (#:uiop
	       #:alexandria
	       #:cl-ansi-text
	       #:terminfo
	       #:xkbcommon
	       #:cl-wayland
	       #:snakes
	       #:iterate
	       #:cffi)
  :in-order-to ((test-op (test-op mahogany-test)))
  :components ((:file "log")
	       (:file "util")
	       (:module bindings
			:components ((:file "package")
				     (:file "hrt-bindings")))
	       (:file "package")
	       (:module interfaces
			:depends-on ("package")
			:components ((:file "view-interface")
				     ))
	       (:module keyboard
			:depends-on ("package" "util")
			:components ((:file "keytrans")
				     (:file "key")
				     (:file "kmap")))
	       (:module tree
			:depends-on ("package" "log" "util" "interfaces")
	       		:components ((:file "tree-interface")
	       			     (:file "frame" :depends-on ("tree-interface"))
				     (:file "view" :depends-on ("tree-interface"))))
	       (:file "main" :depends-on ("bindings" "package"))))

(asdf:defsystem #:mahogany/executable
  :build-operation program-op
  :entry-point "mahogany::run-server"
  :build-pathname "build/mahogany"
  :depends-on (#:mahogany))
