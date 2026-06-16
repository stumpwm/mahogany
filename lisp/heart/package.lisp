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
  (:local-nicknames (#:mh/interface #:mahogany/wm-interface)
                    (#:colors #:cl-colors2))
  (:nicknames #:hrt)
  (:export #:define-hrt-callback
           #:hrt-output-callbacks
           #:hrt-seat-callbacks
           ;; view callbacks:
           #:hrt-view-callbacks
           #:new-view
           #:view-size-changed
           #:hrt-view
           #:view-mapped
           #:view-unmapped
           #:view-set-fullscreen
           #:view-fullscreen-p
           #:request-minimize
           #:request-maximize
           #:request-fullscreen
           #:view-destroyed
           #:view-mapped-p
           #:hrt-seat
           #:hrt-seat-reset-view-under
           #:seat-set-cursor-img
           #:hrt-seat-reset-cursor-img
           #:hrt-seat-notify-button
           #:hrt-seat-notify-axis
           #:hrt-seat-cursor-lx
           #:hrt-seat-cursor-ly
           #:hrt-seat-set-keymap
           ;; output symbols:
           #:output
           #:hrt-output
           #:output-hrt-output
           #:make-output
           #:destroy-output
           #:output-init
           #:make-output-config
           #:output-config
           #:hrt-keypress-info
           ;; output callbacks
           #:output-added
           #:output-removed
           #:output-layout-changed
           ;; output methods:
           #:output-resolution
           #:output-position
           #:output-usable-area
           #:output-full-name
           #:output-name
	   #:output-make
	   #:output-model
	   #:output-serial
           ;; view-methods
           #:view
           #:view-init
           #:view-reparent
           #:view-request-close
           #:view-hrt-view
           #:focus-view
           #:unfocus-view
           #:view-set-hidden
           #:view-configure
           ;; view transactions:
           #:dirty-view-transaction
           #:with-view-transaction
           ;; seat callbacks
           #:button-event #:wheel-event #:keyboard-keypress-event
           ;; Server-related symbols
           #:hrt-server
           #:server-init
           #:server-finish
           #:hrt-server-scene-tree
           #:hrt-server-group-create
           #:hrt-server-seat
           #:server-start
           #:hrt-server-stop
           #:run-in-main-thread
           ;; Toast messages:
           #:toast-message
           #:toast-message-gravity
           ;; keypress info slots:
           #:keysyms
           #:modifiers
           #:keysyms-len
           #:wl-key-state
           ;; subprocesses:
           #:subprocess-collect
           #:collect-output-result
           #:collect-output-result-stdout
           #:collect-output-result-stderr
           #:collect-output-result-exit-code
           ;; scene helpers:
           #:hrt-scene-root-destroy
           #:hrt-scene-group-create
           #:hrt-scene-group-destroy
           #:hrt-scene-output-create
           #:hrt-scene-output-destroy
           #:hrt-scene-layer-create
           #:hrt-scene-layer-destroy
           #:scene-layer-add-view
           #:hrt-scene-group-set-enabled
           #:hrt-scene-layer-transfer
           #:hrt-scene-group-set-dimensions
           #:hrt-scene-group-set-position
           ;; #:hrt-scene-group-layers
           #:scene-create-fullscreen-node
           #:hrt-scene-fullscreen-node-destroy
           #:scene-fullscreen-configure
           #:scene-fullscreen-swap
           #:load-foreign-libraries))
