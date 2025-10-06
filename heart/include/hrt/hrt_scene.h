#ifndef HRT_HRT_SCENE
#define HRT_HRT_SCENE

#include <stdint.h>
#include <stdbool.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>
#include "hrt/hrt_output.h"


struct hrt_scene_group {
    struct wlr_scene_tree *normal;
    struct wlr_scene_tree *fullscreen;
};

struct hrt_scene_fullscreen_node {
    /// the tree holding the background node as well as the view:
    struct wlr_scene_tree *tree;
    struct wlr_scene_rect *background;
    struct hrt_view *view;
};

struct hrt_scene_group *hrt_scene_group_create(struct wlr_scene_tree *parent);

void hrt_scene_group_destroy(struct hrt_scene_group *group);

void hrt_scene_group_add_view(struct hrt_scene_group *group,
                              struct hrt_view *view);

void hrt_scene_group_init_view(struct hrt_scene_group *group,
                               struct hrt_view *view);

void hrt_scene_group_set_enabled(struct hrt_scene_group *group, bool enabled);

void hrt_scene_group_transfer(struct hrt_scene_group *source,
                              struct hrt_scene_group *destination);

struct wlr_scene_tree *hrt_scene_group_normal(struct hrt_scene_group *group);

// It might make more sense for the cl frontend to pass in the x,y, width, and height
// variables instead of the output, but's cleaner this way.
/**
 * Create a hrt_scene_fullscreen_node with a black bacground of the given size.
 * Mode the hrt_view inside the node, removing it from where ever it was in the scene tree.
 **/
struct hrt_scene_fullscreen_node *
hrt_scene_create_fullscreen_node(struct hrt_scene_group *group,
                                 struct hrt_view *view,
                                 struct hrt_output *output);

/**
 * Destroy the given background node, moving the struct hrt_view to the
 * normral layer.
 * Returns the view that was in the node.
 */
struct hrt_view *
hrt_scene_fullscreen_node_destroy(struct hrt_scene_fullscreen_node *node);

uint32_t hrt_scene_node_set_dimensions(struct hrt_scene_fullscreen_node *node,
                                       int width, int height);

void hrt_scene_node_set_position(struct hrt_scene_fullscreen_node *node,
				 int x, int y);

uint32_t hrt_scene_fullscreen_configure(struct hrt_scene_fullscreen_node *group,
                                        struct hrt_output *output);

#endif
