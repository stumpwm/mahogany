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
  (button-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (wheel-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (keyboard-keypress-event :pointer #| function ptr _Bool (struct hrt_seat *, struct hrt_keypress_info *) |#))

(cffi:defcstruct hrt-input
  (wlr-input-device :pointer #| (:struct wlr-input-device) |# )
  (seat (:pointer (:struct hrt-seat)))
  (link (:struct wl-list))
  (destroy (:struct wl-listener)))

(cffi:defcfun ("hrt_seat_init" hrt-seat-init) :bool
  (seat (:pointer (:struct hrt-seat)))
  (server (:pointer (:struct hrt-server)))
  (callbacks (:pointer (:struct hrt-seat-callbacks))))

(cffi:defcfun ("hrt_seat_destroy" hrt-seat-destroy) :void
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_cursor_init" hrt-cursor-init) :bool
  (seat (:pointer (:struct hrt-seat)))
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_cursor_destroy" hrt-cursor-destroy) :void
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_keyboard_init" hrt-keyboard-init) :void
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_keyboard_destroy" hrt-keyboard-destroy) :void
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_seat_set_cursor_img" hrt-seat-set-cursor-img) :void
  "Set the seat's default cursor image to the given cursor name.

Does not take ownership of the string.

See themes section of man xcursor(3) to find where to find valid cursor names."
  (seat (:pointer (:struct hrt-seat)))
  (img-name (:pointer :char)))

;; next section imported from file build/include/hrt/hrt_view.h

(cffi:defcstruct hrt-view)

(cffi:defctype view-destroy-handler :pointer #| function ptr void (struct hrt_view *) |#)

(cffi:defctype new-view-handler :pointer #| function ptr void (struct hrt_view *) |#)

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
  (request-fullscreen (:struct wl-listener))
  (new-view-handler new-view-handler)
  (destroy-handler view-destroy-handler))

(cffi:defcstruct hrt-view-callbacks
  (new-view new-view-handler)
  (view-destroyed view-destroy-handler))

(cffi:defcfun ("hrt_view_init" hrt-view-init) :void
  "Fully initialize the view and place it in the given scene tree."
  (view (:pointer (:struct hrt-view)))
  (tree :pointer #| (:struct wlr-scene-tree) |# ))

(cffi:defcfun ("hrt_view_set_size" hrt-view-set-size) :uint32
  "Request that this view be the given size. Returns the associated configure serial."
  (view (:pointer (:struct hrt-view)))
  (width :int)
  (height :int))

(cffi:defcfun ("hrt_view_set_relative" hrt-view-set-relative) :void
  "Sets the view to the given coordinates relative to its parent."
  (view (:pointer (:struct hrt-view)))
  (x :int)
  (y :int))

(cffi:defcfun ("hrt_view_focus" hrt-view-focus) :void
  "Focus the given view and perform the needed tasks to make
it visible to the user."
  (view (:pointer (:struct hrt-view)))
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_view_unfocus" hrt-view-unfocus) :void
  "Unfocus the given view."
  (view (:pointer (:struct hrt-view)))
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_view_set_hidden" hrt-view-set-hidden) :void
  "Stop the given view from being displayed"
  (view (:pointer (:struct hrt-view)))
  (hidden :bool))

(cffi:defcfun ("hrt_view_reparent" hrt-view-reparent) :void
  (view (:pointer (:struct hrt-view)))
  (node :pointer #| (:struct wlr-scene-tree) |# ))

(cffi:defcfun ("hrt_view_request_close" hrt-view-request-close) :void
  "Request that the view be closed. This is the \"nice\" version
that is the same as clicking the close button on window decorations.
It does not garentee that the application actually closes, but
well behaved ones should."
  (view (:pointer (:struct hrt-view))))

;; next section imported from file build/include/hrt/hrt_output.h

(cffi:defcstruct hrt-output
  (wlr-output :pointer #| (:struct wlr-output) |# )
  (server (:pointer (:struct hrt-server)))
  (request-state (:struct wl-listener))
  (frame (:struct wl-listener))
  (destroy (:struct wl-listener))
  (color :float :count 4))

(cffi:defcstruct hrt-output-callbacks
  (output-added :pointer #| function ptr void (struct hrt_output *) |#)
  (output-removed :pointer #| function ptr void (struct hrt_output *) |#)
  (output-layout-changed :pointer #| function ptr void () |#))

(cffi:defcfun ("hrt_output_init" hrt-output-init) :bool
  (server (:pointer (:struct hrt-server)))
  (callbacks (:pointer (:struct hrt-output-callbacks))))

(cffi:defcfun ("hrt_output_destroy" hrt-output-destroy) :void
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_output_resolution" hrt-output-resolution) :void
  "Get the effective output resolution of the output that can be used to
set the width and height of views."
  (output (:pointer (:struct hrt-output)))
  (width (:pointer :int))
  (height (:pointer :int)))

(cffi:defcfun ("hrt_output_position" hrt-output-position) :void
  (output (:pointer (:struct hrt-output)))
  (x (:pointer :int))
  (y (:pointer :int)))

(cffi:defcfun ("hrt_output_name" hrt-output-name) :string
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_make" hrt-output-make) :string
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_model" hrt-output-model) :string
  (output (:pointer (:struct hrt-output))))

(cffi:defcfun ("hrt_output_serial" hrt-output-serial) :string
  (output (:pointer (:struct hrt-output))))

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
  (xdg-shell :pointer #| (:struct wlr-xdg-shell) |# )
  (new-xdg-toplevel (:struct wl-listener))
  (new-xdg-popup (:struct wl-listener))
  (output-callback (:pointer (:struct hrt-output-callbacks)))
  (view-callbacks (:pointer (:struct hrt-view-callbacks))))

(cffi:defcfun ("hrt_server_init" hrt-server-init) :bool
  (server (:pointer (:struct hrt-server)))
  (output-callbacks (:pointer (:struct hrt-output-callbacks)))
  (seat-callbacks (:pointer (:struct hrt-seat-callbacks)))
  (view-callbacks (:pointer (:struct hrt-view-callbacks)))
  (log-level :int #| enum wlr-log-importance |#))

(cffi:defcfun ("hrt_server_start" hrt-server-start) :bool
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_server_stop" hrt-server-stop) :void
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_server_finish" hrt-server-finish) :void
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_server_scene_tree" hrt-server-scene-tree) :pointer #| (:struct wlr-scene-tree) |# 
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_server_seat" hrt-server-seat) (:pointer (:struct hrt-seat))
  (server (:pointer (:struct hrt-server))))
