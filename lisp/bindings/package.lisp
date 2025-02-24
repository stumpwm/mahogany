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
	   #:view-reparent
	   #:view-request-close
	   #:view-hrt-view
	   #:focus-view
	   #:unfocus-view
	   #:view-set-hidden
	   ;; seat callbacks
	   #:button-event #:wheel-event #:keyboard-keypress-event
	   #:hrt-server
	   #:hrt-server-scene-tree
	   #:hrt-server-seat
	   #:hrt-server-init
	   #:hrt-server-start
	   #:hrt-server-stop
	   #:hrt-server-finish
	   ;; keypress info slots:
	   #:keysyms
	   #:modifiers
	   #:keysyms-len
	   #:wl-key-state
	   #:load-foreign-libraries))

(defpackage #:wlr
  (:use :cl #:wayland-server-core)
  (:export
   #:scene-tree-create
   #:scene-node-destroy
   #:scene-node-set-position
   #:scene-node-reparent
   #:scene-node-set-enabled
   ;; scene node slot:
   #:node))
