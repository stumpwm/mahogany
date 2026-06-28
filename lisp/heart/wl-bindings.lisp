(in-package #:wl)

(cffi:defcstruct wl-list
  (prev :pointer)
  (next :pointer))

(cffi:defcstruct wl-listener
  "wl_listener struct"
  (link (:struct wl-list))
  (notify :pointer))

(cffi:defcenum wl-output-transform
  +output-transform-normal+
  +output-transform-90+
  +output-transform-180+
  +output-transform-270+
  +output-transform-flipped+
  +output-transform-flipped-90+
  +output-transform-flipped-180+
  +output-transform-flipped-270+)

(cffi:defcenum wl-keyboard-key-state
  +wl-keyboard-key-state-released+
  +wl-keyboard-key-state-pressed+
  +wl-keyboard-key-state-repeated+)

(cffi:defcenum zwlr-layer-surface-v1-keyboard-interactivity
  (:keyboard-interactivity-none 0)
  (:keyboard-interactivity-exclusive 1)
  (:keyboard-interactivity-on-demand 2))
