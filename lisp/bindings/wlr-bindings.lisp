(cl:in-package #:wlr)

;; next section imported from file heart/subprojects/wlroots/include/wlr/util/box.h

(cffi:defcstruct wlr-box
  "A box representing a rectangle region in a 2D space.

The x and y coordinates are inclusive, and the width and height lengths are
exclusive. In other words, the box starts from the coordinates (x, y), and
goes up to but not including (x + width, y + height)."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcstruct wlr-fbox
  "A floating-point box representing a rectangle region in a 2D space.

struct wlr_fbox has the same semantics as struct wlr_box."
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(declaim (inline wlr-box-closest-point))
(cffi:defcfun ("wlr_box_closest_point" wlr-box-closest-point) :void
  "Finds the closest point within the box bounds.

Returns NAN if the box is empty."
  (box :pointer #| (:struct wlr-box) |# )
  (x :double)
  (y :double)
  (dest-x (:pointer :double))
  (dest-y (:pointer :double)))

(declaim (inline wlr-box-intersection))
(cffi:defcfun ("wlr_box_intersection" wlr-box-intersection) :bool
  "Gives the intersecting box between two struct wlr_box.

Returns an empty box if the provided boxes don't intersect."
  (dest :pointer #| (:struct wlr-box) |# )
  (box-a :pointer #| (:struct wlr-box) |# )
  (box-b :pointer #| (:struct wlr-box) |# ))

(declaim (inline wlr-box-contains-point))
(cffi:defcfun ("wlr_box_contains_point" wlr-box-contains-point) :bool
  "Verifies if a point is contained within the bounds of a given struct wlr_box.

For example:

- A point at (100, 50) is not contained in the box (0, 0, 100, 50).
- A point at (10, 10) is contained in the box (10, 0, 50, 50)."
  (box :pointer #| (:struct wlr-box) |# )
  (x :double)
  (y :double))

(declaim (inline wlr-box-contains-box))
(cffi:defcfun ("wlr_box_contains_box" wlr-box-contains-box) :bool
  "Verifies that a box is fully contained within another box.

Returns true if the \"smaller\" box is fully contained within the \"bigger\" box.
If either of the boxes are empty, false is returned."
  (bigger :pointer #| (:struct wlr-box) |# )
  (smaller :pointer #| (:struct wlr-box) |# ))

(declaim (inline wlr-box-empty))
(cffi:defcfun ("wlr_box_empty" wlr-box-empty) :bool
  "Checks whether a box is empty or not.

A box is considered empty if its width and/or height is zero or negative."
  (box :pointer #| (:struct wlr-box) |# ))

(declaim (inline wlr-box-transform))
(cffi:defcfun ("wlr_box_transform" wlr-box-transform) :void
  "Transforms a box inside a (0, 0, width, height) box."
  (dest :pointer #| (:struct wlr-box) |# )
  (box :pointer #| (:struct wlr-box) |# )
  (transform :int #| enum wl-output-transform |#)
  (width :int)
  (height :int))

(declaim (inline wlr-fbox-empty))
(cffi:defcfun ("wlr_fbox_empty" wlr-fbox-empty) :bool
  "Checks whether a box is empty or not.

A box is considered empty if its width and/or height is zero or negative."
  (box :pointer #| (:struct wlr-fbox) |# ))

(declaim (inline wlr-fbox-transform))
(cffi:defcfun ("wlr_fbox_transform" wlr-fbox-transform) :void
  "Transforms a floating-point box inside a (0, 0, width, height) box."
  (dest :pointer #| (:struct wlr-fbox) |# )
  (box :pointer #| (:struct wlr-fbox) |# )
  (transform :int #| enum wl-output-transform |#)
  (width :double)
  (height :double))

(declaim (inline wlr-box-equal))
(cffi:defcfun ("wlr_box_equal" wlr-box-equal) :bool
  "Returns true if the two boxes are equal, false otherwise."
  (a :pointer #| (:struct wlr-box) |# )
  (b :pointer #| (:struct wlr-box) |# ))

(declaim (inline wlr-fbox-equal))
(cffi:defcfun ("wlr_fbox_equal" wlr-fbox-equal) :bool
  "Returns true if the two boxes are equal, false otherwise."
  (a :pointer #| (:struct wlr-fbox) |# )
  (b :pointer #| (:struct wlr-fbox) |# ))
