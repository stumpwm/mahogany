#ifndef HRT_HRT_SCENE
#define HRT_HRT_SCENE

#include <stdint.h>
#include <stdbool.h>
#include <hrt/hrt_output.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

struct hrt_output;

struct hrt_scene_root {
    struct wlr_scene_tree *background;
    struct wlr_scene_tree *bottom;
    struct wlr_scene_tree *normal;
    struct wlr_scene_tree *fullscreen;
    struct wlr_scene_tree *top;
    struct wlr_scene_tree *overlay;
    // Should we store the outputs and groups associated with this?
    struct {
      struct wl_listener scene_destroy;
    } listeners;
};

struct hrt_scene_output {
    struct wlr_scene_tree *background;
    struct wlr_scene_tree *bottom;

    struct wlr_scene_tree *top;
    struct wlr_scene_tree *overlay;
};

struct hrt_scene_group {
    struct wlr_scene_tree *layers;
    struct wlr_scene_tree *fullscreens;
};

struct hrt_scene_layer {
  struct wlr_scene_tree *tree;
};

struct hrt_scene_fullscreen_layer {
    /// the tree holding the background node as well as the view:
    struct hrt_scene_layer layer;
    struct wlr_scene_rect *background;
    struct hrt_view *view;
};

void hrt_scene_root_destroy(struct hrt_scene_root *scene_root);

struct hrt_scene_output *hrt_scene_output_create(struct hrt_scene_root *scene);

void hrt_scene_output_destroy(struct hrt_scene_output *output);

void hrt_scene_group_destroy(struct hrt_scene_group *group);

void hrt_scene_group_set_enabled(struct hrt_scene_group *group, bool enabled);

struct hrt_scene_layer *hrt_scene_layer_create(struct hrt_scene_group *group);

void hrt_scene_layer_destroy(struct hrt_scene_layer *layer);

void hrt_scene_layer_add_view(struct hrt_scene_layer *layer,
                              struct hrt_view *view);

/**
 * Transfer all of the views in the source layer to the
 * destination layer
 **/
void hrt_scene_layer_transfer(struct hrt_scene_layer *source,
                              struct hrt_scene_layer *destination);

struct wlr_scene_tree *hrt_scene_group_layers(struct hrt_scene_group *group);

// It might make more sense for the cl frontend to pass in the x,y, width, and height
// variables instead of the output, but's cleaner this way.
/**
 * Create a hrt_scene_fullscreen_layer with a black bacground of the given size.
 * Mode the hrt_view inside the node, removing it from where ever it was in the scene tree.
 **/
struct hrt_scene_fullscreen_layer *
hrt_scene_create_fullscreen_layer(struct hrt_scene_group *group,
                                 struct hrt_view *view,
                                 struct hrt_output *output);

/**
 * Destroy the given background node, moving the struct hrt_view to the
 * normral layer.
 * Returns the view that was in the node.
 */
struct hrt_view *
hrt_scene_fullscreen_layer_destroy(struct hrt_scene_fullscreen_layer *node);

uint32_t hrt_scene_node_set_dimensions(struct hrt_scene_fullscreen_layer *node,
                                       int width, int height);

void hrt_scene_node_set_position(struct hrt_scene_fullscreen_layer *node, int x,
                                 int y);

uint32_t hrt_scene_fullscreen_configure(struct hrt_scene_fullscreen_layer *group,
                                        struct hrt_output *output);

#endif
