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
	       #:adopt
	       #:xkbcommon
	       #:cl-wayland
	       #:iterate
	       #:cffi)
  :in-order-to ((test-op (test-op mahogany-test)))
  :pathname #p"lisp/"
  :components ((:file "log")
	       (:file "util")
	       (:file "system" :depends-on ("util" "config" "log"))
               (:module config
			:components ((:file "config-system")))
	       (:module ring-list
			:components ((:file "ring-list")))
	       (:module interfaces
			:components ((:file "view-interface")))
	       (:module bindings
		        :serial t
			:depends-on ("interfaces")
			:components ((:file "package")
				     (:file "wlr-bindings")
				     (:file "hrt-libs")
				     (:file "hrt-bindings")
				     (:file "hrt-debug" :if-feature :hrt-debug)
				     (:file "wrappers")
				     (:file "view")
				     (:file "scene-group")))
	       (:module keyboard
			:depends-on ("util")
		        :components ((:file "package")
				     (:file "keytrans")
				     (:file "key")
				     (:file "kmap")))
	       (:module tree
			:depends-on ("log" "util" "interfaces")
	       	        :components ((:file "package")
				     (:file "tree-interface")
				     (:file "output-node" :depends-on ("tree-interface"))
	       			     (:file "frame" :depends-on ("tree-interface"))
				     (:file "view" :depends-on ("tree-interface"))))
	       (:file "package")
	       (:file "objects" :depends-on ("package" "ring-list"))
	       (:file "group" :depends-on ("transaction" "objects" "bindings"))
	       (:file "state" :depends-on ("objects" "transaction" "keyboard"))
		   (:file "globals" :depends-on ("objects" "system"))
		   (:file "kmap-modes" :depends-on ("objects" "globals" "keyboard"))
		   (:file "transaction" :depends-on ("globals"))
	       (:file "output" :depends-on ("objects" "bindings" "state"))
	       (:file "events" :depends-on ("globals" "state" "objects" "bindings"))
	       (:file "input" :depends-on ("state" "keyboard" "bindings"))
	       (:file "key-bindings" :depends-on ("kmap-modes" "state" "tree" "input"))
	       (:file "main" :depends-on ("bindings" "keyboard" "input" "package"))))

(asdf:defsystem #:mahogany/executable
  :build-operation program-op
  :entry-point "mahogany::main"
  :build-pathname "build/mahogany"
  :depends-on (#:mahogany))
