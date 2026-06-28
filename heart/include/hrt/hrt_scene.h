#ifndef HRT_HRT_SCENE
#define HRT_HRT_SCENE

#include <stdint.h>
#include <stdbool.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_layer_shell_v1.h>

struct hrt_output;

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
    struct hrt_output *output;
};

struct hrt_scene_group {
    struct wlr_scene_tree *layers;
};

struct hrt_scene_fullscreen_node {
    /// the tree holding the background node as well as the view:
    struct wlr_scene_tree *layer;
    struct wlr_scene_rect *background;
    struct hrt_view *view;
};

void hrt_scene_root_destroy(struct hrt_scene_root *scene_root);

struct hrt_scene_output *hrt_scene_output_create(struct hrt_scene_root *scene);

void hrt_scene_output_destroy(struct hrt_scene_output *output);

struct wlr_scene_tree *
hrt_scene_output_get_layer(struct hrt_scene_output *output,
                           enum zwlr_layer_shell_v1_layer layer_type);

void hrt_scene_group_destroy(struct hrt_scene_group *group);

void hrt_scene_group_set_enabled(struct hrt_scene_group *group, bool enabled);

struct wlr_scene_tree *hrt_scene_layer_create(struct hrt_scene_group *group);

void hrt_scene_layer_destroy(struct wlr_scene_tree *layer);

void hrt_scene_layer_add_view(struct wlr_scene_tree *layer,
                              struct hrt_view *view);

/**
 * Transfer all of the views in the source layer to the
 * destination layer
 **/
void hrt_scene_layer_transfer(struct wlr_scene_tree *source,
                              struct wlr_scene_tree *destination);

struct wlr_scene_tree *hrt_scene_group_layers(struct hrt_scene_group *group);

// It might make more sense for the cl frontend to pass in the x,y, width, and height
// variables instead of the output, but's cleaner this way.
/**
 * Create a hrt_scene_fullscreen_layer with a black bacground of the given size.
 * Mode the hrt_view inside the node, removing it from where ever it was in the scene tree.
 **/
struct hrt_scene_fullscreen_node *
hrt_scene_create_fullscreen_node(struct wlr_scene_tree *layer,
                                 struct hrt_view *view,
                                 struct hrt_output *output);

struct hrt_view *hrt_scene_fullscreen_swap(struct hrt_scene_fullscreen_node *node,
                               struct hrt_view *view);


/**
 * Destroy the given background node, moving the struct hrt_view to the
 * normral layer.
 * Returns the view that was in the node.
 */
struct hrt_view *
hrt_scene_fullscreen_node_destroy(struct hrt_scene_fullscreen_node *node);

uint32_t hrt_scene_node_set_dimensions(struct hrt_scene_fullscreen_node *node,
                                       int width, int height);

void hrt_scene_node_set_position(struct hrt_scene_fullscreen_node *node, int x,
                                 int y);

uint32_t hrt_scene_fullscreen_configure(struct hrt_scene_fullscreen_node *group,
                                        struct hrt_output *output);

#endif
