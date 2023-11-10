(defpackage #:mahogany/core
  (:use :cl #:wayland-server-core #:xkb)
  (:nicknames #:hrt)
  (:export #:hrt-output-callbacks
	   #:hrt-seat-callbacks
	   #:hrt-view-callbacks
	   #:new-view
	   #:hrt-view
	   #:hrt-view-init
	   #:view-destroyed
	   #:hrt-seat
	   #:hrt-output
	   #:hrt-keypress-info
	   ;; output callbacks
	   #:output-added
	   #:output-removed
	   #:output-mode-changed
	   ;; output methods:
	   #:output-resolution
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
