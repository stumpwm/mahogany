(defpackage #:wayland
  (:use :cl)
  (:nicknames #:wl)
  (:export #:wl-list
           #:wl-listener
           #:wl-output-transform
           #:wl-keyboard-key-state
           #:+wl-keyboard-key-state-released+
           #:+wl-keyboard-key-state-pressed+
           #:+wl-keyboard-key-state-repeated+
           #:zwlr-layer-surface-v1-keyboard-interactivity
           #:zwlr-layer-shell-v1-layer))

(defpackage #:wlr
  (:use :cl #:wayland)
  (:export
   ;; Symbols for wlr-box:
   #:wlr-box
   #:x
   #:y
   #:width
   #:height
   #:wlr-log-importance))

(defpackage #:mahogany/core
  (:use :cl #:wayland #:xkb #:wlr)
  (:local-nicknames (#:mh/interface #:mahogany/wm-interface)
                    (#:colors #:cl-colors2))
  (:nicknames #:hrt)
  (:export #:define-hrt-callback
           #:hrt-output-callbacks
           #:hrt-seat-callbacks
           ;; view callbacks:
           #:hrt-view-callbacks
           #:hrt-layer-shell-callbacks
           #:new-layer-surface
           #:layer-surface-mapped
           #:layer-surface-unmapped
           #:layers-reconfigured
           #:make-layer-surface
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
           ;; border box
           #:border-box-set-style
           #:border-box-style-create
           #:border-box-style-update
           #:border-box-create
           #:hrt-border-box-destroy
           #:hrt-border-box-set-size
           #:hrt-border-box-set-relative
           #:hrt-border-box-set-enabled
           ;; Seat
           #:hrt-seat
           #:hrt-seat-reset-view-under
           #:seat-grabbed-p
           #:seat-grab
           #:hrt-seat-ungrab
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
           #:output-config-merge
           #:hrt-keypress-info
           ;; output callbacks
           #:output-added
           #:output-removed
           #:output-layout-changed
           ;; output methods:
           #:output-scene
           #:output-scene-layer
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
           #:server-group-create
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
           #:scene-layer-create
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
           #:load-foreign-libraries
           ;; layer shell methods
           #:layer-surface-output
           #:hrt-layer-surface-output
           #:layer-surface
           #:layer-surface-keyboard-interactivity
           #:layer-surface-position
           #:layer-surface-dimensions
           #:layer-changed
           #:keyboard-interactivity-updated
           #:hrt-layer-shell-surface-set-output
           #:hrt-layer-shell-surface-abort
           #:layer-shell-surface-place
           #:hrt-layer-shell-finish-init))
