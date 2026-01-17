(cl:in-package #:hrt)

;; next section imported from file build/include/hrt/hrt_input.h

(cffi:defcstruct hrt-server)

(cffi:defcstruct hrt-seat-callbacks)

(cffi:defcstruct hrt-seat
  (server (:pointer (:struct hrt-server)))
  (cursor :pointer #| (:struct wlr-cursor) |# )
  (keyboard-group :pointer #| (:struct wlr-keyboard-group) |# )
  (xcursor-manager :pointer #| (:struct wlr-xcursor-manager) |# )
  (seat :pointer #| (:struct wlr-seat) |# )
  (inputs (:struct wl-list))
  (new-input (:struct wl-listener))
  (xkb-context :pointer #| (:struct xkb-context) |# )
  (motion (:struct wl-listener))
  (motion-absolute (:struct wl-listener))
  (button (:struct wl-listener))
  (axis (:struct wl-listener))
  (frame (:struct wl-listener))
  (request-cursor (:struct wl-listener))
  (keyboard-key (:struct wl-listener))
  (keyboard-modifiers (:struct wl-listener))
  (callbacks (:pointer (:struct hrt-seat-callbacks)))
  (cursor-image (:pointer :char)))

(cffi:defcstruct hrt-keypress-info
  (keysyms :pointer #| xkb-keysym-t |# )
  (modifiers :uint32)
  (keysyms-len :size)
  (wl-key-state :int #| enum wl-keyboard-key-state |#))

(cffi:defcstruct hrt-seat-callbacks
  (button-event :pointer #| function ptr void (struct hrt_seat *, struct wlr_pointer_button_event *) |#)
  (wheel-event :pointer #| function ptr void (struct hrt_seat *, struct wlr_pointer_axis_event *) |#)
  (keyboard-keypress-event :pointer #| function ptr _Bool (struct hrt_seat *, struct hrt_keypress_info *) |#))

(cffi:defcstruct hrt-input
  (wlr-input-device :pointer #| (:struct wlr-input-device) |# )
  (seat (:pointer (:struct hrt-seat)))
  (link (:struct wl-list))
  (destroy (:struct wl-listener)))

(declaim (inline hrt-seat-reset-view-under))
(cffi:defcfun ("hrt_seat_reset_view_under" hrt-seat-reset-view-under) :void
  "Send the appropriate cursor event to the view under the
cursor."
  (seat (:pointer (:struct hrt-seat))))

(declaim (inline hrt-seat-set-cursor-img))
(cffi:defcfun ("hrt_seat_set_cursor_img" hrt-seat-set-cursor-img) :void
  "Set the seat's default cursor image to the given cursor name.

Does not take ownership of the string.

See themes section of man xcursor(3) to find where to find valid cursor
names."
  (seat (:pointer (:struct hrt-seat)))
  (img-name (:pointer :char)))

(declaim (inline hrt-seat-notify-button))
(cffi:defcfun ("hrt_seat_notify_button" hrt-seat-notify-button) :void
  (seat (:pointer (:struct hrt-seat)))
  (event :pointer #| (:struct wlr-pointer-button-event) |# ))

(declaim (inline hrt-seat-notify-axis))
(cffi:defcfun ("hrt_seat_notify_axis" hrt-seat-notify-axis) :void
  (seat (:pointer (:struct hrt-seat)))
  (event :pointer #| (:struct wlr-pointer-axis-event) |# ))

(declaim (inline hrt-seat-set-keymap))
(cffi:defcfun ("hrt_seat_set_keymap" hrt-seat-set-keymap) :void
  (seat (:pointer (:struct hrt-seat)))
  (rules :pointer #| (:struct xkb-rule-names) |# )
  (flags xkb:keymap-compile-flags #| enum xkb-keymap-compile-flags |#))

(declaim (inline hrt-seat-cursor-lx))
(cffi:defcfun ("hrt_seat_cursor_lx" hrt-seat-cursor-lx) :double
  (seat (:pointer (:struct hrt-seat))))

(declaim (inline hrt-seat-cursor-ly))
(cffi:defcfun ("hrt_seat_cursor_ly" hrt-seat-cursor-ly) :double
  (seat (:pointer (:struct hrt-seat))))

;; next section imported from file build/include/hrt/hrt_view.h

(cffi:defcstruct hrt-view)

(cffi:defctype view-destroy-handler :pointer #| function ptr void (struct hrt_view *) |#)

(cffi:defctype new-view-handler :pointer #| function ptr void (struct hrt_view *) |#)

(cffi:defctype view-mapped-handler :pointer #| function ptr void (struct hrt_view *) |#)

(cffi:defctype view-request-fullscreen :pointer #| function ptr _Bool (struct hrt_view *, struct hrt_output *, _Bool) |#)

(cffi:defcstruct hrt-view-callbacks
  (new-view new-view-handler)
  (view-mapped view-mapped-handler)
  (view-unmapped view-mapped-handler)
  (request-minimize view-mapped-handler)
  (request-maximize view-mapped-handler)
  (view-destroyed view-destroy-handler)
  (request-fullscreen view-request-fullscreen))

(cffi:defcstruct hrt-view
  (width :int)
  (height :int)
  (xdg-surface :pointer #| (:struct wlr-xdg-surface) |# )
  (xdg-toplevel :pointer #| (:struct wlr-xdg-toplevel) |# )
  (scene-tree :pointer #| (:struct wlr-scene-tree) |# )
  (map (:struct wl-listener))
  (unmap (:struct wl-listener))
  (commit (:struct wl-listener))
  (destroy (:struct wl-listener))
  (request-maximize (:struct wl-listener))
  (request-minimize (:struct wl-listener))
  (request-fullscreen (:struct wl-listener))
  (callbacks (:pointer (:struct hrt-view-callbacks))))

(declaim (inline hrt-view-init))
(cffi:defcfun ("hrt_view_init" hrt-view-init) :void
  "Fully initialize the view and place it in the given scene tree."
  (view (:pointer (:struct hrt-view)))
  (tree :pointer #| (:struct wlr-scene-tree) |# ))

(declaim (inline hrt-view-info))
(cffi:defcfun ("hrt_view_info" hrt-view-info) :void
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-view-set-size))
(cffi:defcfun ("hrt_view_set_size" hrt-view-set-size) :uint32
  "Request that this view be the given size. Returns the associated configure
serial."
  (view (:pointer (:struct hrt-view)))
  (width :int)
  (height :int))

(declaim (inline hrt-view-mapped))
(cffi:defcfun ("hrt_view_mapped" hrt-view-mapped) :bool
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-view-set-relative))
(cffi:defcfun ("hrt_view_set_relative" hrt-view-set-relative) :void
  "Sets the view to the given coordinates relative to its parent."
  (view (:pointer (:struct hrt-view)))
  (x :int)
  (y :int))

(declaim (inline hrt-view-focus))
(cffi:defcfun ("hrt_view_focus" hrt-view-focus) :void
  "Focus the given view and perform the needed tasks to make
it visible to the user."
  (view (:pointer (:struct hrt-view)))
  (seat (:pointer (:struct hrt-seat))))

(declaim (inline hrt-view-unfocus))
(cffi:defcfun ("hrt_view_unfocus" hrt-view-unfocus) :void
  "Unfocus the given view."
  (view (:pointer (:struct hrt-view)))
  (seat (:pointer (:struct hrt-seat))))

(declaim (inline hrt-view-set-hidden))
(cffi:defcfun ("hrt_view_set_hidden" hrt-view-set-hidden) :void
  "Stop the given view from being displayed"
  (view (:pointer (:struct hrt-view)))
  (hidden :bool))

(declaim (inline hrt-view-reparent))
(cffi:defcfun ("hrt_view_reparent" hrt-view-reparent) :void
  (view (:pointer (:struct hrt-view)))
  (node :pointer #| (:struct wlr-scene-tree) |# ))

(declaim (inline hrt-view-request-close))
(cffi:defcfun ("hrt_view_request_close" hrt-view-request-close) :void
  "Request that the view be closed. This is the \"nice\" version
that is the same as clicking the close button on window decorations.
It does not garentee that the application actually closes, but
well behaved ones should."
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-view-send-configure))
(cffi:defcfun ("hrt_view_send_configure" hrt-view-send-configure) :void
  "Send a configure event to the view"
  (view (:pointer (:struct hrt-view))))

;; next section imported from file build/include/hrt/hrt_output.h

(cffi:defcstruct hrt-output
  (wlr-output :pointer #| (:struct wlr-output) |# )
  (server (:pointer (:struct hrt-server)))
  (scene :pointer #| (:struct hrt-scene-output) |#)
  (wlr-scene :pointer #| (:struct wlr-scene-output) |# )
  (usable-area (:struct wlr-box))
  (request-state (:struct wl-listener))
  (frame (:struct wl-listener))
  (destroy (:struct wl-listener))
  (color :float :count 4))

(cffi:defcstruct hrt-output-callbacks
  (output-added :pointer #| function ptr void (struct hrt_output *) |#)
  (output-removed :pointer #| function ptr void (struct hrt_output *) |#)
  (output-layout-changed :pointer #| function ptr void () |#))

(declaim (inline hrt-output-resolution))
(cffi:defcfun ("hrt_output_resolution" hrt-output-resolution) :void
  "Get the effective output resolution of the output that can be used to
set the width and height of views."
  (output (:pointer (:struct hrt-output)))
  (width (:pointer :int))
  (height (:pointer :int)))

(declaim (inline hrt-output-position))
(cffi:defcfun ("hrt_output_position" hrt-output-position) :void
  (output (:pointer (:struct hrt-output)))
  (x (:pointer :int))
  (y (:pointer :int)))

(declaim (inline hrt-output-name))
(cffi:defcfun ("hrt_output_name" hrt-output-name) :string
  (output (:pointer (:struct hrt-output))))

(declaim (inline hrt-output-make))
(cffi:defcfun ("hrt_output_make" hrt-output-make) :string
  (output (:pointer (:struct hrt-output))))

(declaim (inline hrt-output-model))
(cffi:defcfun ("hrt_output_model" hrt-output-model) :string
  (output (:pointer (:struct hrt-output))))

(declaim (inline hrt-output-serial))
(cffi:defcfun ("hrt_output_serial" hrt-output-serial) :string
  (output (:pointer (:struct hrt-output))))

;; next section imported from file build/include/hrt/hrt_scene.h

(cffi:defcstruct hrt-scene-root-listeners
  (scene-destroy (:struct wl-listener)))

(cffi:defcstruct hrt-scene-root
  (background :pointer #| (:struct wlr-scene-tree) |# )
  (bottom :pointer #| (:struct wlr-scene-tree) |# )
  (normal :pointer #| (:struct wlr-scene-tree) |# )
  (fullscreen :pointer #| (:struct wlr-scene-tree) |# )
  (top :pointer #| (:struct wlr-scene-tree) |# )
  (overlay :pointer #| (:struct wlr-scene-tree) |# )
  (listeners (:struct hrt-scene-root-listeners)))

(cffi:defcstruct hrt-scene-output
  (background :pointer #| (:struct wlr-scene-tree) |# )
  (bottom :pointer #| (:struct wlr-scene-tree) |# )
  (top :pointer #| (:struct wlr-scene-tree) |# )
  (overlay :pointer #| (:struct wlr-scene-tree) |# ))

(cffi:defcstruct hrt-scene-group
  (normal :pointer #| (:struct wlr-scene-tree) |# )
  (fullscreen :pointer #| (:struct wlr-scene-tree) |# ))

(cffi:defcstruct hrt-scene-fullscreen-node
  (tree :pointer #| (:struct wlr-scene-tree) |# )
  (background :pointer #| (:struct wlr-scene-rect) |# )
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-scene-root-destroy))
(cffi:defcfun ("hrt_scene_root_destroy" hrt-scene-root-destroy) :void
  (scene-root (:pointer (:struct hrt-scene-root))))

(declaim (inline hrt-scene-output-create))
(cffi:defcfun ("hrt_scene_output_create" hrt-scene-output-create) (:pointer (:struct hrt-scene-output))
  (scene (:pointer (:struct hrt-scene-root))))

(declaim (inline hrt-scene-output-destroy))
(cffi:defcfun ("hrt_scene_output_destroy" hrt-scene-output-destroy) :void
  (output (:pointer (:struct hrt-scene-output))))

(declaim (inline hrt-scene-group-destroy))
(cffi:defcfun ("hrt_scene_group_destroy" hrt-scene-group-destroy) :void
  (group (:pointer (:struct hrt-scene-group))))

(declaim (inline hrt-scene-group-add-view))
(cffi:defcfun ("hrt_scene_group_add_view" hrt-scene-group-add-view) :void
  (group (:pointer (:struct hrt-scene-group)))
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-scene-group-init-view))
(cffi:defcfun ("hrt_scene_group_init_view" hrt-scene-group-init-view) :void
  (group (:pointer (:struct hrt-scene-group)))
  (view (:pointer (:struct hrt-view))))

(declaim (inline hrt-scene-group-set-enabled))
(cffi:defcfun ("hrt_scene_group_set_enabled" hrt-scene-group-set-enabled) :void
  (group (:pointer (:struct hrt-scene-group)))
  (enabled :bool))

(declaim (inline hrt-scene-group-transfer))
(cffi:defcfun ("hrt_scene_group_transfer" hrt-scene-group-transfer) :void
  (source (:pointer (:struct hrt-scene-group)))
  (destination (:pointer (:struct hrt-scene-group))))

(declaim (inline hrt-scene-group-normal))
(cffi:defcfun ("hrt_scene_group_normal" hrt-scene-group-normal) :pointer #| (:struct wlr-scene-tree) |#
  (group (:pointer (:struct hrt-scene-group))))

(declaim (inline hrt-scene-create-fullscreen-node))
(cffi:defcfun ("hrt_scene_create_fullscreen_node" hrt-scene-create-fullscreen-node) (:pointer (:struct hrt-scene-fullscreen-node))
  "Create a hrt_scene_fullscreen_node with a black bacground of the given size.
Mode the hrt_view inside the node, removing it from where ever it was in the scene tree."
  (group (:pointer (:struct hrt-scene-group)))
  (view (:pointer (:struct hrt-view)))
  (output (:pointer (:struct hrt-output))))

(declaim (inline hrt-scene-fullscreen-node-destroy))
(cffi:defcfun ("hrt_scene_fullscreen_node_destroy" hrt-scene-fullscreen-node-destroy) (:pointer (:struct hrt-view))
  "Destroy the given background node, moving the struct hrt_view to the
normral layer.
Returns the view that was in the node."
  (node (:pointer (:struct hrt-scene-fullscreen-node))))

(declaim (inline hrt-scene-node-set-dimensions))
(cffi:defcfun ("hrt_scene_node_set_dimensions" hrt-scene-node-set-dimensions) :uint32
  (node (:pointer (:struct hrt-scene-fullscreen-node)))
  (width :int)
  (height :int))

(declaim (inline hrt-scene-node-set-position))
(cffi:defcfun ("hrt_scene_node_set_position" hrt-scene-node-set-position) :void
  (node (:pointer (:struct hrt-scene-fullscreen-node)))
  (x :int)
  (y :int))

(declaim (inline hrt-scene-fullscreen-configure))
(cffi:defcfun ("hrt_scene_fullscreen_configure" hrt-scene-fullscreen-configure) :uint32
  (group (:pointer (:struct hrt-scene-fullscreen-node)))
  (output (:pointer (:struct hrt-output))))

;; next section imported from file build/include/hrt/hrt_message.h

(cffi:defcenum window-gravity
  (:gravity-top-right 0)
  (:gravity-top-left 1)
  (:gravity-bottom-right 2)
  (:gravity-bottom-left 3)
  (:gravity-right 4)
  (:gravity-left 5)
  (:gravity-top 6)
  (:gravity-bottom 7)
  (:gravity-center 8)
  (:gravity-max 8))

(cffi:defcstruct hrt-message-theme
  (font (:pointer :char))
  (font-color :float :count 4)
  (background-color :float :count 4)
  (border-color :float :count 4)
  (message-padding :int)
  (message-border-width :int)
  (margin-x :int)
  (margin-y :int))

(declaim (inline hrt-toast-message))
(cffi:defcfun ("hrt_toast_message" hrt-toast-message) :bool
  (server (:pointer (:struct hrt-server)))
  (output (:pointer (:struct hrt-output)))
  (text (:pointer :char))
  (gravity :int #| enum window-gravity |#)
  (theme (:pointer (:struct hrt-message-theme)))
  (ms-delay :int))

;; next section imported from file build/include/hrt/hrt_server.h

(cffi:defcstruct hrt-server
  (wl-display :pointer #| (:struct wl-display) |# )
  (backend :pointer #| (:struct wlr-backend) |# )
  (backend-destroy (:struct wl-listener))
  (session :pointer #| (:struct wlr-session) |# )
  (renderer :pointer #| (:struct wlr-renderer) |# )
  (compositor :pointer #| (:struct wlr-compositor) |# )
  (allocator :pointer #| (:struct wlr-allocator) |# )
  (scene :pointer #| (:struct wlr-scene) |# )
  (scene-layout :pointer #| (:struct wlr-scene-output-layout) |# )
  (new-output (:struct wl-listener))
  (output-manager :pointer #| (:struct wlr-output-manager-v1) |# )
  (output-layout :pointer #| (:struct wlr-output-layout) |# )
  (output-layout-changed (:struct wl-listener))
  (output-manager-apply (:struct wl-listener))
  (output-manager-test (:struct wl-listener))
  (output-manager-destroy (:struct wl-listener))
  (seat (:struct hrt-seat))
  (scene-root (:pointer (:struct hrt-scene-root)))
  (xdg-shell :pointer #| (:struct wlr-xdg-shell) |# )
  (new-xdg-toplevel (:struct wl-listener))
  (new-xdg-popup (:struct wl-listener))
  (output-callback (:pointer (:struct hrt-output-callbacks)))
  (view-callbacks (:pointer (:struct hrt-view-callbacks)))
  (message-timer-source :pointer #| (:struct wl-event-source) |# )
  (message-buffer :pointer #| (:struct wlr-scene-buffer) |# ))

(declaim (inline hrt-server-init))
(cffi:defcfun ("hrt_server_init" hrt-server-init) :bool
  (server (:pointer (:struct hrt-server)))
  (output-callbacks (:pointer (:struct hrt-output-callbacks)))
  (seat-callbacks (:pointer (:struct hrt-seat-callbacks)))
  (view-callbacks (:pointer (:struct hrt-view-callbacks)))
  (log-level :int #| enum wlr-log-importance |#))

(declaim (inline hrt-server-start))
(cffi:defcfun ("hrt_server_start" hrt-server-start) :bool
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-stop))
(cffi:defcfun ("hrt_server_stop" hrt-server-stop) :void
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-finish))
(cffi:defcfun ("hrt_server_finish" hrt-server-finish) :void
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-scene-tree))
(cffi:defcfun ("hrt_server_scene_tree" hrt-server-scene-tree) :pointer #| (:struct wlr-scene-tree) |# 
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-seat))
(cffi:defcfun ("hrt_server_seat" hrt-server-seat) (:pointer (:struct hrt-seat))
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-group-create))
(cffi:defcfun ("hrt_server_group_create" hrt-server-group-create) (:pointer (:struct hrt-scene-group))
  (server (:pointer (:struct hrt-server))))

(declaim (inline hrt-server-struct-size))
(cffi:defcfun ("hrt_server_struct_size" hrt-server-struct-size) :size)
