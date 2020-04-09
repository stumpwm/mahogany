;;;; mahogany.asd

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :depends-on (#:uiop #:alexandria #:cl-ppcre #:bordeaux-threads
		      #:cl-ansi-text #:terminfo
		      #:cl-xkeysym
		      #:snakes #:iterate)
  :in-order-to ((test-op (test-op mahogany-test)))
  :components ((:file "log")
	       (:file "util")
	       (:file "package")
	       (:module interfaces
			:depends-on ("package")
			:components ((:file "view-interface")
				     (:file "wm-interface")
				     (:file "backend-interface")))
	       (:module tree
			:depends-on ("package" "log" "util" "interfaces")
	       		:components ((:file "tree-interface")
	       			     (:file "frame" :depends-on ("tree-interface"))
				     (:file "view" :depends-on ("tree-interface"))))))
