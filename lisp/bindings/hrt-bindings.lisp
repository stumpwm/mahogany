(cl:in-package #:hrt)

;; next section imported from file build/include/hrt/hrt_view.h

(cffi:defcstruct hrt-view)

(cffi:defctype view-destroy-handler :pointer #| function ptr void (struct hrt_view *) |#)

(cffi:defcstruct hrt-view
  (xdg-surface :pointer #| (:struct wlr-xdg-surface) |# )
  (xdg-toplevel :pointer #| (:struct wlr-xdg-toplevel) |# )
  (scene-tree :pointer #| (:struct wlr-scene-tree) |# )
  (map (:struct wl-listener))
  (unmap (:struct wl-listener))
  (destroy (:struct wl-listener))
  (destroy-handler view-destroy-handler))

(cffi:defcstruct hrt-view-callbacks
  (new-view :pointer #| function ptr void (struct hrt_view *) |#)
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

;; next section imported from file build/include/hrt/hrt_input.h

(cffi:defcstruct hrt-server)

(cffi:defcstruct hrt-seat-callbacks
  (button-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (wheel-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (keyboard-keypress-event :pointer #| function ptr _Bool (struct hrt_seat *, struct hrt_keypress_info *) |#))

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
  (keyboard-key (:struct wl-listener))
  (keyboard-modifiers (:struct wl-listener))
  (callbacks (:pointer (:struct hrt-seat-callbacks)))
  (cursor-image (:pointer :char)))

(cffi:defcstruct hrt-keypress-info
  (keysyms :pointer #| xkb-keysym-t |# )
  (modifiers :uint32)
  (keysyms-len :size))

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

(cffi:defcfun ("hrt_cursor_init" hrt-cursor-init) :bool
  (seat (:pointer (:struct hrt-seat)))
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_keyboard_init" hrt-keyboard-init) :void
  (seat (:pointer (:struct hrt-seat))))

(cffi:defcfun ("hrt_seat_set_cursor_img" hrt-seat-set-cursor-img) :void
  "Set the seat's default cursor image to the given cursor name.

Does not take ownership of the string.

See themes section of man xcursor(3) to find where to find valid cursor names."
  (seat (:pointer (:struct hrt-seat)))
  (img-name (:pointer :char)))

;; next section imported from file build/include/hrt/hrt_output.h

(cffi:defcstruct hrt-output
  (wlr-output :pointer #| (:struct wlr-output) |# )
  (server (:pointer (:struct hrt-server)))
  (link (:struct wl-list))
  (frame (:struct wl-listener))
  (destroy (:struct wl-listener))
  (color :float :count 4))

(cffi:defcstruct hrt-output-callbacks
  (output-added :pointer #| function ptr void (struct hrt_output *) |#)
  (output-removed :pointer #| function ptr void (struct hrt_output *) |#))

(cffi:defcfun ("hrt_output_init" hrt-output-init) :bool
  (server (:pointer (:struct hrt-server)))
  (callbacks (:pointer (:struct hrt-output-callbacks))))

;; next section imported from file build/include/hrt/hrt_server.h

(cffi:defcstruct hrt-server
  (wl-display :pointer #| (:struct wl-display) |# )
  (backend :pointer #| (:struct wlr-backend) |# )
  (renderer :pointer #| (:struct wlr-renderer) |# )
  (compositor :pointer #| (:struct wlr-compositor) |# )
  (allocator :pointer #| (:struct wlr-allocator) |# )
  (scene :pointer #| (:struct wlr-scene) |# )
  (outputs (:struct wl-list))
  (new-output (:struct wl-listener))
  (output-manager :pointer #| (:struct wlr-output-manager-v1) |# )
  (output-layout :pointer #| (:struct wlr-output-layout) |# )
  (output-manager-apply (:struct wl-listener))
  (output-manager-test (:struct wl-listener))
  (seat (:struct hrt-seat))
  (xdg-shell :pointer #| (:struct wlr-xdg-shell) |# )
  (new-xdg-surface (:struct wl-listener))
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
