(cl:in-package #:hrt)

;; next section imported from file build/include/hrt/hrt_group.h

#| MACRO_DEFINITION
(defconstant +hrt-group+ ACTUAL_VALUE_HERE)
|#

(cffi:defcstruct hrt-group
  (name (:pointer :char))
  (scene :pointer)
  (views (:struct wl-list)))

;; next section imported from file build/include/hrt/hrt_input.h

#| MACRO_DEFINITION
(defconstant +hrt-hrt-input-h+ ACTUAL_VALUE_HERE)
|#

(cffi:defcstruct hrt-server)

(cffi:defcstruct hrt-seat-callbacks
  (button-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (wheel-event :pointer #| function ptr void (struct hrt_seat *) |#)
  (keyboard-keypress-event :pointer #| function ptr _Bool (struct hrt_seat *, struct hrt_keypress_info *) |#))

(cffi:defcstruct hrt-seat
  (server (:pointer (:struct hrt-server)))
  (cursor :pointer)
  (keyboard-group :pointer)
  (xcursor-manager :pointer)
  (seat :pointer)
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
  (keysyms (:pointer xkb:keysym))
  (modifiers :uint32)
  (keysyms-len :size))

(cffi:defcstruct hrt-input
  (wlr-input-device :pointer)
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

#| MACRO_DEFINITION
(defconstant +hrt-hrt-output-h+ ACTUAL_VALUE_HERE)
|#

(cffi:defcstruct hrt-output
  (wlr-output :pointer)
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

#| MACRO_DEFINITION
(defconstant +hrt-hrt-server-h+ ACTUAL_VALUE_HERE)
|#

(cffi:defcstruct hrt-server
  (wl-display :pointer)
  (backend :pointer)
  (renderer :pointer)
  (compositor :pointer)
  (allocator :pointer)
  (outputs (:struct wl-list))
  (new-output (:struct wl-listener))
  (output-manager :pointer)
  (output-layout :pointer)
  (output-manager-apply (:struct wl-listener))
  (output-manager-test (:struct wl-listener))
  (seat (:struct hrt-seat))
  (output-callback (:pointer (:struct hrt-output-callbacks))))

(cffi:defcfun ("hrt_server_init" hrt-server-init) :bool
  (server (:pointer (:struct hrt-server)))
  (output-callbacks (:pointer (:struct hrt-output-callbacks)))
  (seat-callbacks (:pointer (:struct hrt-seat-callbacks)))
  (log-level :int))

(cffi:defcfun ("hrt_server_start" hrt-server-start) :bool
  (server (:pointer (:struct hrt-server))))

(cffi:defcfun ("hrt_server_finish" hrt-server-finish) :void
  (server (:pointer (:struct hrt-server))))
