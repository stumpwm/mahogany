(cl:in-package #:wlr)

;; next section imported from file heart/subprojects/wlroots/include/wlr/util/addon.h

(cffi:defcstruct addon-set
  (addons (:struct wl-list)))

(cffi:defcstruct addon)

(cffi:defcstruct addon-interface
  (name (:pointer :char))
  (destroy :pointer #| function ptr void (struct wlr_addon *) |#))

(cffi:defcstruct addon
  (impl :pointer #| (:struct addon-interface) |# )
  (owner (:pointer :void))
  (link (:struct wl-list)))

(cffi:defcfun ("wlr_addon_set_init" addon-set-init) :void
  (set :pointer #| (:struct addon-set) |# ))

(cffi:defcfun ("wlr_addon_set_finish" addon-set-finish) :void
  (set :pointer #| (:struct addon-set) |# ))

(cffi:defcfun ("wlr_addon_init" addon-init) :void
  (addon :pointer #| (:struct addon) |# )
  (set :pointer #| (:struct addon-set) |# )
  (owner (:pointer :void))
  (impl :pointer #| (:struct addon-interface) |# ))

(cffi:defcfun ("wlr_addon_finish" addon-finish) :void
  (addon :pointer #| (:struct addon) |# ))

(cffi:defcfun ("wlr_addon_find" addon-find) :pointer #| (:struct addon) |#
  (set :pointer #| (:struct addon-set) |# )
  (owner (:pointer :void))
  (impl :pointer #| (:struct addon-interface) |# ))

;; next section imported from file heart/subprojects/wlroots/include/wlr/util/box.h

(cffi:defcstruct box
  "A box representing a rectangle region in a 2D space.

The x and y coordinates are inclusive, and the width and height lengths are
exclusive. In other words, the box starts from the coordinates (x, y), and
goes up to but not including (x + width, y + height)."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcstruct fbox
  "A floating-point box representing a rectangle region in a 2D space.

struct wlr_fbox has the same semantics as struct wlr_box."
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(cffi:defcfun ("wlr_box_closest_point" box-closest-point) :void
  "Finds the closest point within the box bounds.

Returns NAN if the box is empty."
  (box :pointer #| (:struct box) |# )
  (x :double)
  (y :double)
  (dest-x (:pointer :double))
  (dest-y (:pointer :double)))

(cffi:defcfun ("wlr_box_intersection" box-intersection) :bool
  "Gives the intersecting box between two struct wlr_box.

Returns an empty box if the provided boxes don't intersect."
  (dest :pointer #| (:struct box) |# )
  (box-a :pointer #| (:struct box) |# )
  (box-b :pointer #| (:struct box) |# ))

(cffi:defcfun ("wlr_box_contains_point" box-contains-point) :bool
  "Verifies if a point is contained within the bounds of a given struct wlr_box.

For example:

- A point at (100, 50) is not contained in the box (0, 0, 100, 50).
- A point at (10, 10) is contained in the box (10, 0, 50, 50)."
  (box :pointer #| (:struct box) |# )
  (x :double)
  (y :double))

(cffi:defcfun ("wlr_box_empty" box-empty) :bool
  "Checks whether a box is empty or not.

A box is considered empty if its width and/or height is zero or negative."
  (box :pointer #| (:struct box) |# ))

(cffi:defcfun ("wlr_box_transform" box-transform) :void
  "Transforms a box inside a (0, 0, width, height) box."
  (dest :pointer #| (:struct box) |# )
  (box :pointer #| (:struct box) |# )
  (transform :int #| enum wl-output-transform |#)
  (width :int)
  (height :int))

(cffi:defcfun ("wlr_fbox_empty" fbox-empty) :bool
  "Checks whether a box is empty or not.

A box is considered empty if its width and/or height is zero or negative."
  (box :pointer #| (:struct fbox) |# ))

(cffi:defcfun ("wlr_fbox_transform" fbox-transform) :void
  "Transforms a floating-point box inside a (0, 0, width, height) box."
  (dest :pointer #| (:struct fbox) |# )
  (box :pointer #| (:struct fbox) |# )
  (transform :int #| enum wl-output-transform |#)
  (width :double)
  (height :double))

(cffi:defcfun ("wlr_box_equal" box-equal) :bool
  "Returns true if the two boxes are equal, false otherwise."
  (a :pointer #| (:struct box) |# )
  (b :pointer #| (:struct box) |# ))

(cffi:defcfun ("wlr_fbox_equal" fbox-equal) :bool
  "Returns true if the two boxes are equal, false otherwise."
  (a :pointer #| (:struct fbox) |# )
  (b :pointer #| (:struct fbox) |# ))

;; next section imported from file heart/subprojects/wlroots/include/wlr/types/wlr_scene.h

(cffi:defcstruct output)

(cffi:defcstruct output-layout)

(cffi:defcstruct output-layout-output)

(cffi:defcstruct xdg-surface)

(cffi:defcstruct layer-surface-v1)

(cffi:defcstruct drag-icon)

(cffi:defcstruct surface)

(cffi:defcstruct scene-node
  "A node is an object in the scene. */")

(cffi:defcstruct scene-buffer
  "A scene-graph node displaying a buffer */")

(cffi:defcstruct scene-output-layout)

(cffi:defcstruct presentation)

(cffi:defcstruct linux-dmabuf-v1)

(cffi:defcstruct output-state)

(cffi:defctype scene-buffer-point-accepts-input-func-t :pointer #| function ptr _Bool (struct wlr_scene_buffer *, double *, double *) |#)

(cffi:defctype scene-buffer-iterator-func-t :pointer #| function ptr void (struct wlr_scene_buffer *, int, int, void *) |#)

(cffi:defcenum scene-node-type
  (:wlr-scene-node-tree 0)
  (:wlr-scene-node-rect 1)
  (:wlr-scene-node-buffer 2))

(cffi:defcstruct scene-node-events
  (destroy (:struct wl-signal)))

(cffi:defcstruct scene-node
  ;; This entire thing is commented out because we don't have the pixman type available:
  ;; "A node is an object in the scene. */"
  ;; (type :int #| enum scene-node-type |#)
  ;; (parent :pointer #| (:struct scene-tree) |# )
  ;; (link (:struct wl-list))
  ;; (enabled :bool)
  ;; (x :int)
  ;; (y :int)
  ;; (events (:struct scene-node-events))
  ;; (data (:pointer :void))
  ;; (addons (:struct addon-set))
  ;; (visible pixman-region32-t)
  )

(cffi:defcenum scene-debug-damage-option
  (:wlr-scene-debug-damage-none 0)
  (:wlr-scene-debug-damage-rerender 1)
  (:wlr-scene-debug-damage-highlight 2))

(cffi:defcstruct scene-tree
  "A sub-tree in the scene-graph. */"
  (node (:struct scene-node))
  (children (:struct wl-list)))

(cffi:defcstruct scene
  "The root scene-graph node. */"
  (tree (:struct scene-tree))
  (outputs (:struct wl-list))
  (linux-dmabuf-v1 :pointer #| (:struct linux-dmabuf-v1) |# )
  (linux-dmabuf-v1-destroy (:struct wl-listener))
  (debug-damage-option :int #| enum scene-debug-damage-option |#)
  (direct-scanout :bool)
  (calculate-visibility :bool)
  (highlight-transparent-region :bool))

(cffi:defcstruct scene-surface
  "A scene-graph node displaying a single surface. */"
  (buffer :pointer #| (:struct scene-buffer) |# )
  (surface :pointer #| (:struct surface) |# )
  (clip (:struct box))
  (addon (:struct addon))
  (outputs-update (:struct wl-listener))
  (output-enter (:struct wl-listener))
  (output-leave (:struct wl-listener))
  (output-sample (:struct wl-listener))
  (frame-done (:struct wl-listener))
  (surface-destroy (:struct wl-listener))
  (surface-commit (:struct wl-listener)))

(cffi:defcstruct scene-rect
  "A scene-graph node displaying a solid-colored rectangle */"
  (node (:struct scene-node))
  (width :int)
  (height :int)
  (color :float :count 4))

(cffi:defcstruct scene-outputs-update-event
  (active :pointer #| :pointer #| (:struct scene-output) |#  |# )
  (size :size))

(cffi:defcstruct scene-output-sample-event
  (output :pointer #| (:struct scene-output) |# )
  (direct-scanout :bool))

(cffi:defcstruct scene-buffer
  "A scene-graph node displaying a buffer */")

(cffi:defcstruct scene-output-events
  (destroy (:struct wl-signal)))

(cffi:defcstruct scene-output
  "A viewport for an output in the scene-graph */"
  ;; (output :pointer #| (:struct output) |# )
  ;; (link (:struct wl-list))
  ;; (scene :pointer #| (:struct scene) |# )
  ;; (addon (:struct addon))
  ;; (damage-ring (:struct damage-ring))
  ;; (x :int)
  ;; (y :int)
  ;; (events (:struct scene-output-events))
  ;; (pending-commit-damage pixman-region32-t)
  ;; (index :uint8)
  ;; (prev-scanout :bool)
  ;; (output-commit (:struct wl-listener))
  ;; (output-damage (:struct wl-listener))
  ;; (output-needs-frame (:struct wl-listener))
  ;; (damage-highlight-regions (:struct wl-list))
  ;; (render-list (:struct wl-array))
  )

(cffi:defcstruct scene-timer
  (pre-render-duration :int64)
  (render-timer :pointer #| (:struct render-timer) |# ))

(cffi:defcstruct scene-layer-surface-v1
  "A layer shell scene helper */"
  (tree :pointer #| (:struct scene-tree) |# )
  (layer-surface :pointer #| (:struct layer-surface-v1) |# )
  (tree-destroy (:struct wl-listener))
  (layer-surface-destroy (:struct wl-listener))
  (layer-surface-map (:struct wl-listener))
  (layer-surface-unmap (:struct wl-listener)))

(cffi:defcfun ("wlr_scene_node_destroy" scene-node-destroy) :void
  "Immediately destroy the scene-graph node."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_node_set_enabled" scene-node-set-enabled) :void
  "Enable or disable this node. If a node is disabled, all of its children are
implicitly disabled as well."
  (node :pointer #| (:struct scene-node) |# )
  (enabled :bool))

(cffi:defcfun ("wlr_scene_node_set_position" scene-node-set-position) :void
  "Set the position of the node relative to its parent."
  (node :pointer #| (:struct scene-node) |# )
  (x :int)
  (y :int))

(cffi:defcfun ("wlr_scene_node_place_above" scene-node-place-above) :void
  "Move the node right above the specified sibling.
Asserts that node and sibling are distinct and share the same parent."
  (node :pointer #| (:struct scene-node) |# )
  (sibling :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_node_place_below" scene-node-place-below) :void
  "Move the node right below the specified sibling.
Asserts that node and sibling are distinct and share the same parent."
  (node :pointer #| (:struct scene-node) |# )
  (sibling :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_node_raise_to_top" scene-node-raise-to-top) :void
  "Move the node above all of its sibling nodes."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_node_lower_to_bottom" scene-node-lower-to-bottom) :void
  "Move the node below all of its sibling nodes."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_node_reparent" scene-node-reparent) :void
  "Move the node to another location in the tree."
  (node :pointer #| (:struct scene-node) |# )
  (new-parent :pointer #| (:struct scene-tree) |# ))

(cffi:defcfun ("wlr_scene_node_coords" scene-node-coords) :bool
  "Get the node's layout-local coordinates.

True is returned if the node and all of its ancestors are enabled."
  (node :pointer #| (:struct scene-node) |# )
  (lx (:pointer :int))
  (ly (:pointer :int)))

(cffi:defcfun ("wlr_scene_node_for_each_buffer" scene-node-for-each-buffer) :void
  "Call `iterator` on each buffer in the scene-graph, with the buffer's
position in layout coordinates. The function is called from root to leaves
(in rendering order)."
  (node :pointer #| (:struct scene-node) |# )
  (iterator scene-buffer-iterator-func-t)
  (user-data (:pointer :void)))

(cffi:defcfun ("wlr_scene_node_at" scene-node-at) :pointer #| (:struct scene-node) |#
  "Find the topmost node in this scene-graph that contains the point at the
given layout-local coordinates. (For surface nodes, this means accepting
input events at that point.) Returns the node and coordinates relative to the
returned node, or NULL if no node is found at that location."
  (node :pointer #| (:struct scene-node) |# )
  (lx :double)
  (ly :double)
  (nx (:pointer :double))
  (ny (:pointer :double)))

(cffi:defcfun ("wlr_scene_create" scene-create) :pointer #| (:struct scene) |#
  "Create a new scene-graph.")

(cffi:defcfun ("wlr_scene_set_linux_dmabuf_v1" scene-set-linux-dmabuf-v1) :void
  "Handles linux_dmabuf_v1 feedback for all surfaces in the scene.

Asserts that a struct wlr_linux_dmabuf_v1 hasn't already been set for the scene."
  (scene :pointer #| (:struct scene) |# )
  (linux-dmabuf-v1 :pointer #| (:struct linux-dmabuf-v1) |# ))

(cffi:defcfun ("wlr_scene_tree_create" scene-tree-create) :pointer #| (:struct scene-tree) |#
  "Add a node displaying nothing but its children."
  (parent :pointer #| (:struct scene-tree) |# ))

(cffi:defcfun ("wlr_scene_surface_create" scene-surface-create) :pointer #| (:struct scene-surface) |#
  "Add a node displaying a single surface to the scene-graph.

The child sub-surfaces are ignored.

wlr_surface_send_enter() and wlr_surface_send_leave() will be called
automatically based on the position of the surface and outputs in
the scene."
  (parent :pointer #| (:struct scene-tree) |# )
  (surface :pointer #| (:struct surface) |# ))

(cffi:defcfun ("wlr_scene_buffer_from_node" scene-buffer-from-node) :pointer #| (:struct scene-buffer) |#
  "If this node represents a wlr_scene_buffer, that buffer will be returned. It
is not legal to feed a node that does not represent a wlr_scene_buffer."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_tree_from_node" scene-tree-from-node) :pointer #| (:struct scene-tree) |#
  "If this node represents a wlr_scene_tree, that tree will be returned. It
is not legal to feed a node that does not represent a wlr_scene_tree."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_rect_from_node" scene-rect-from-node) :pointer #| (:struct scene-rect) |#
  "If this node represents a wlr_scene_rect, that rect will be returned. It
is not legal to feed a node that does not represent a wlr_scene_rect."
  (node :pointer #| (:struct scene-node) |# ))

(cffi:defcfun ("wlr_scene_surface_try_from_buffer" scene-surface-try-from-buffer) :pointer #| (:struct scene-surface) |#
  "If this buffer is backed by a surface, then the struct wlr_scene_surface is
returned. If not, NULL will be returned."
  (scene-buffer :pointer #| (:struct scene-buffer) |# ))

(cffi:defcfun ("wlr_scene_rect_create" scene-rect-create) :pointer #| (:struct scene-rect) |#
  "Add a node displaying a solid-colored rectangle to the scene-graph."
  (parent :pointer #| (:struct scene-tree) |# )
  (width :int)
  (height :int)
  (color :float :count 4))

(cffi:defcfun ("wlr_scene_rect_set_size" scene-rect-set-size) :void
  "Change the width and height of an existing rectangle node."
  (rect :pointer #| (:struct scene-rect) |# )
  (width :int)
  (height :int))

(cffi:defcfun ("wlr_scene_rect_set_color" scene-rect-set-color) :void
  "Change the color of an existing rectangle node."
  (rect :pointer #| (:struct scene-rect) |# )
  (color :float :count 4))

(cffi:defcfun ("wlr_scene_buffer_create" scene-buffer-create) :pointer #| (:struct scene-buffer) |#
  "Add a node displaying a buffer to the scene-graph.

If the buffer is NULL, this node will not be displayed."
  (parent :pointer #| (:struct scene-tree) |# )
  (buffer :pointer #| (:struct buffer) |# ))

(cffi:defcfun ("wlr_scene_buffer_set_buffer" scene-buffer-set-buffer) :void
  "Sets the buffer's backing buffer.

If the buffer is NULL, the buffer node will not be displayed."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (buffer :pointer #| (:struct buffer) |# ))

(cffi:defcfun ("wlr_scene_buffer_set_buffer_with_damage" scene-buffer-set-buffer-with-damage) :void
  "Sets the buffer's backing buffer with a custom damage region.

The damage region is in buffer-local coordinates. If the region is NULL,
the whole buffer node will be damaged."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (buffer :pointer #| (:struct buffer) |# )
  (region :pointer #| pixman-region32-t |# ))

(cffi:defcfun ("wlr_scene_buffer_set_opaque_region" scene-buffer-set-opaque-region) :void
  "Sets the buffer's opaque region. This is an optimization hint used to
determine if buffers which reside under this one need to be rendered or not."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (region :pointer #| pixman-region32-t |# ))

(cffi:defcfun ("wlr_scene_buffer_set_source_box" scene-buffer-set-source-box) :void
  "Set the source rectangle describing the region of the buffer which will be
sampled to render this node. This allows cropping the buffer.

If NULL, the whole buffer is sampled. By default, the source box is NULL."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (box :pointer #| (:struct fbox) |# ))

(cffi:defcfun ("wlr_scene_buffer_set_dest_size" scene-buffer-set-dest-size) :void
  "Set the destination size describing the region of the scene-graph the buffer
will be painted onto. This allows scaling the buffer.

If zero, the destination size will be the buffer size. By default, the
destination size is zero."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (width :int)
  (height :int))

(cffi:defcfun ("wlr_scene_buffer_set_transform" scene-buffer-set-transform) :void
  "Set a transform which will be applied to the buffer."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (transform :int #| enum wl-output-transform |#))

(cffi:defcfun ("wlr_scene_buffer_set_opacity" scene-buffer-set-opacity) :void
  "Sets the opacity of this buffer"
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (opacity :float))

(cffi:defcfun ("wlr_scene_buffer_set_filter_mode" scene-buffer-set-filter-mode) :void
  "Sets the filter mode to use when scaling the buffer"
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (filter-mode :int #| enum scale-filter-mode |#))

(cffi:defcfun ("wlr_scene_buffer_send_frame_done" scene-buffer-send-frame-done) :void
  "Calls the buffer's frame_done signal."
  (scene-buffer :pointer #| (:struct scene-buffer) |# )
  (now :pointer #| (:struct timespec) |# ))

(cffi:defcfun ("wlr_scene_output_create" scene-output-create) :pointer #| (:struct scene-output) |#
  "Add a viewport for the specified output to the scene-graph.

An output can only be added once to the scene-graph."
  (scene :pointer #| (:struct scene) |# )
  (output :pointer #| (:struct output) |# ))

(cffi:defcfun ("wlr_scene_output_destroy" scene-output-destroy) :void
  "Destroy a scene-graph output."
  (scene-output :pointer #| (:struct scene-output) |# ))

(cffi:defcfun ("wlr_scene_output_set_position" scene-output-set-position) :void
  "Set the output's position in the scene-graph."
  (scene-output :pointer #| (:struct scene-output) |# )
  (lx :int)
  (ly :int))

(cffi:defcstruct scene-output-state-options
  (timer :pointer #| (:struct scene-timer) |# )
  (color-transform :pointer #| (:struct color-transform) |# )
  (swapchain :pointer #| (:struct swapchain) |# ))

(cffi:defcfun ("wlr_scene_output_commit" scene-output-commit) :bool
  "Render and commit an output."
  (scene-output :pointer #| (:struct scene-output) |# )
  (options :pointer #| (:struct scene-output-state-options) |# ))

(cffi:defcfun ("wlr_scene_output_build_state" scene-output-build-state) :bool
  "Render and populate given output state."
  (scene-output :pointer #| (:struct scene-output) |# )
  (state :pointer #| (:struct output-state) |# )
  (options :pointer #| (:struct scene-output-state-options) |# ))

(cffi:defcfun ("wlr_scene_timer_get_duration_ns" scene-timer-get-duration-ns) :int64
  "Retrieve the duration in nanoseconds between the last wlr_scene_output_commit() call and the end
of its operations, including those on the GPU that may have finished after the call returned.

Returns -1 if the duration is unavailable."
  (timer :pointer #| (:struct scene-timer) |# ))

(cffi:defcfun ("wlr_scene_timer_finish" scene-timer-finish) :void
  (timer :pointer #| (:struct scene-timer) |# ))

(cffi:defcfun ("wlr_scene_output_send_frame_done" scene-output-send-frame-done) :void
  "Call wlr_surface_send_frame_done() on all surfaces in the scene rendered by
wlr_scene_output_commit() for which wlr_scene_surface.primary_output
matches the given scene_output."
  (scene-output :pointer #| (:struct scene-output) |# )
  (now :pointer #| (:struct timespec) |# ))

(cffi:defcfun ("wlr_scene_output_for_each_buffer" scene-output-for-each-buffer) :void
  "Call `iterator` on each buffer in the scene-graph visible on the output,
with the buffer's position in layout coordinates. The function is called
from root to leaves (in rendering order)."
  (scene-output :pointer #| (:struct scene-output) |# )
  (iterator scene-buffer-iterator-func-t)
  (user-data (:pointer :void)))

(cffi:defcfun ("wlr_scene_get_scene_output" scene-get-scene-output) :pointer #| (:struct scene-output) |#
  "Get a scene-graph output from a struct wlr_output.

If the output hasn't been added to the scene-graph, returns NULL."
  (scene :pointer #| (:struct scene) |# )
  (output :pointer #| (:struct output) |# ))

(cffi:defcfun ("wlr_scene_attach_output_layout" scene-attach-output-layout) :pointer #| (:struct scene-output-layout) |#
  "Attach an output layout to a scene.

The resulting scene output layout allows to synchronize the positions of scene
outputs with the positions of corresponding layout outputs.

It is automatically destroyed when the scene or the output layout is destroyed."
  (scene :pointer #| (:struct scene) |# )
  (output-layout :pointer #| (:struct output-layout) |# ))

(cffi:defcfun ("wlr_scene_output_layout_add_output" scene-output-layout-add-output) :void
  "Add an output to the scene output layout.

When the layout output is repositioned, the scene output will be repositioned
accordingly."
  (sol :pointer #| (:struct scene-output-layout) |# )
  (lo :pointer #| (:struct output-layout-output) |# )
  (so :pointer #| (:struct scene-output) |# ))

(cffi:defcfun ("wlr_scene_subsurface_tree_create" scene-subsurface-tree-create) :pointer #| (:struct scene-tree) |#
  "Add a node displaying a surface and all of its sub-surfaces to the
scene-graph."
  (parent :pointer #| (:struct scene-tree) |# )
  (surface :pointer #| (:struct surface) |# ))

(cffi:defcfun ("wlr_scene_subsurface_tree_set_clip" scene-subsurface-tree-set-clip) :void
  "Sets a cropping region for any subsurface trees that are children of this
scene node. The clip coordinate space will be that of the root surface of
the subsurface tree.

A NULL or empty clip will disable clipping"
  (node :pointer #| (:struct scene-node) |# )
  (clip :pointer #| (:struct box) |# ))

(cffi:defcfun ("wlr_scene_xdg_surface_create" scene-xdg-surface-create) :pointer #| (:struct scene-tree) |#
  "Add a node displaying an xdg_surface and all of its sub-surfaces to the
scene-graph.

The origin of the returned scene-graph node will match the top-left corner
of the xdg_surface window geometry."
  (parent :pointer #| (:struct scene-tree) |# )
  (xdg-surface :pointer #| (:struct xdg-surface) |# ))

(cffi:defcfun ("wlr_scene_layer_surface_v1_create" scene-layer-surface-v1-create) :pointer #| (:struct scene-layer-surface-v1) |#
  "Add a node displaying a layer_surface_v1 and all of its sub-surfaces to the
scene-graph.

The origin of the returned scene-graph node will match the top-left corner
of the layer surface."
  (parent :pointer #| (:struct scene-tree) |# )
  (layer-surface :pointer #| (:struct layer-surface-v1) |# ))

(cffi:defcfun ("wlr_scene_layer_surface_v1_configure" scene-layer-surface-v1-configure) :void
  "Configure a layer_surface_v1, position its scene node in accordance to its
current state, and update the remaining usable area.

full_area represents the entire area that may be used by the layer surface
if its exclusive_zone is -1, and is usually the output dimensions.
usable_area represents what remains of full_area that can be used if
exclusive_zone is >= 0. usable_area is updated if the surface has a positive
exclusive_zone, so that it can be used for the next layer surface."
  (scene-layer-surface :pointer #| (:struct scene-layer-surface-v1) |# )
  (full-area :pointer #| (:struct box) |# )
  (usable-area :pointer #| (:struct box) |# ))

(cffi:defcfun ("wlr_scene_drag_icon_create" scene-drag-icon-create) :pointer #| (:struct scene-tree) |#
  "Add a node displaying a drag icon and all its sub-surfaces to the
scene-graph."
  (parent :pointer #| (:struct scene-tree) |# )
  (drag-icon :pointer #| (:struct drag-icon) |# ))
