(defpackage #:wlr
  (:use :cl #:wayland-server-core)
  (:export
   ;; Symbols for wlr-box:
   #:wlr-box
   #:x
   #:y
   #:width
   #:height))

(defpackage #:mahogany/core
  (:use :cl #:wayland-server-core #:xkb #:wlr)
  (:local-nicknames (#:mh/interface #:mahogany/wm-interface))
  (:nicknames #:hrt)
  (:export #:hrt-output-callbacks
           #:hrt-seat-callbacks
           #:hrt-view-callbacks
           #:new-view
           #:hrt-view
           #:view-mapped
           #:view-unmapped
           #:request-minimize
           #:request-maximize
           #:request-fullscreen
           #:view-destroyed
           #:view-mapped-p
           #:hrt-seat
           #:hrt-seat-reset-view-under
           #:hrt-seat-notify-button
           #:hrt-seat-notify-axis
           #:hrt-seat-cursor-lx
           #:hrt-seat-cursor-ly
           #:hrt-seat-set-keymap
           ;; output symbols:
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
           #:output-usable-area
           ;; view-methods
           #:view
           #:view-reparent
           #:view-request-close
           #:view-hrt-view
           #:focus-view
           #:unfocus-view
           #:view-set-hidden
           #:view-configure
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
           ;; scene helpers:
           #:hrt-scene-root-create
           #:hrt-scene-root-destroy
           #:hrt-scene-group-create
           #:hrt-scene-group-destroy
           #:hrt-scene-output-create
           #:hrt-scene-output-destroy
           #:scene-group-add-view
           #:hrt-scene-group-set-enabled
           #:hrt-scene-group-transfer
           #:hrt-scene-group-set-dimensions
           #:hrt-scene-group-set-position
           ;; #:hrt-scene-group-normal
           #:scene-create-fullscreen-node
           #:hrt-scene-fullscreen-node-destroy
           #:hrt-scene-fullscreen-configure
           #:scene-init-view
           #:load-foreign-libraries))
