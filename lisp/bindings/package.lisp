(defpackage #:mahogany/core
  (:use :cl #:wayland-server-core #:xkb)
  (:local-nicknames (#:mh/interface #:mahogany/wm-interface))
  (:nicknames #:hrt)
  (:export #:hrt-output-callbacks
	   #:hrt-seat-callbacks
	   #:hrt-view-callbacks
	   #:new-view
	   #:hrt-view
	   #:view-destroyed
	   #:hrt-seat
	   #:hrt-output
	   #:hrt-output-name
	   #:hrt-output-make
	   #:hrt-output-model
	   #:hrt-output-serial
	   #:hrt-keypress-info
	   ;; output callbacks
	   #:output-added
	   #:output-removed
	   #:output-layout-changed
	   ;; output methods:
	   #:output-resolution
	   #:output-position
	   ;; view-methods
	   #:view
	   #:view-init
	   #:view-hrt-view
	   ;; seat callbacks
	   #:button-event #:wheel-event #:keyboard-keypress-event
	   #:hrt-server
	   #:hrt-server-scene-tree
	   #:hrt-server-init
	   #:hrt-server-start
	   #:hrt-server-stop
	   #:hrt-server-finish
	   #:keysyms
	   #:modifiers
	   #:keysyms-len
	   #:load-foreign-libraries))
