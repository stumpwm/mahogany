#include "hrt/hrt_output.h"
#include "hrt/hrt_scene.h"
#include "hrt/hrt_server.h"
#include "hrt/hrt_view.h"
#include "wlr/util/log.h"
#include <stdlib.h>
#include <time.h>
#include <wayland-util.h>

static struct wlr_scene_tree *create_place_above(struct wlr_scene_tree *parent,
                                                 struct wlr_scene_tree *below) {
    struct wlr_scene_tree *tree = wlr_scene_tree_create(parent);
    wlr_scene_node_place_above(&tree->node, &below->node);
    return tree;
}

struct hrt_scene_group *hrt_scene_group_create(struct wlr_scene_tree *parent) {
    struct hrt_scene_group *layers = calloc(1, sizeof(*layers));
    if (!layers) {
        wlr_log(WLR_ERROR, "Could not allocate hrt_scene_layers");
        return NULL;
    }
    layers->normal     = wlr_scene_tree_create(parent);
    layers->fullscreen = create_place_above(parent, layers->normal);

    return layers;
}

void hrt_scene_group_destroy(struct hrt_scene_group *layers) {
    wlr_scene_node_destroy(&layers->fullscreen->node);
    wlr_scene_node_destroy(&layers->normal->node);
    free(layers);
}

void hrt_scene_group_add_view(struct hrt_scene_group *group,
                              struct hrt_view *view) {
    hrt_view_reparent(view, group->normal);
}

void hrt_scene_group_init_view(struct hrt_scene_group *group,
                               struct hrt_view *view) {
    hrt_view_init(view, group->normal);
}

void hrt_scene_group_set_enabled(struct hrt_scene_group *group, bool enabled) {
    wlr_scene_node_set_enabled(&group->fullscreen->node, enabled);
    wlr_scene_node_set_enabled(&group->normal->node, enabled);
}

static void reparent_children(struct wlr_scene_tree *source,
                              struct wlr_scene_tree *dest) {
    struct wlr_scene_node *child, *child_tmp;
    wl_list_for_each_safe(child, child_tmp, &source->children, link) {
        wlr_scene_node_reparent(child, dest);
    }
}

void hrt_scene_group_transfer(struct hrt_scene_group *source,
                              struct hrt_scene_group *destination) {
    reparent_children(source->fullscreen, destination->fullscreen);
    reparent_children(source->normal, destination->normal);
}

struct wlr_scene_tree *hrt_scene_group_normal(struct hrt_scene_group *group) {
    return group->normal;
}

struct hrt_scene_fullscreen_node *
hrt_scene_create_fullscreen_node(struct hrt_scene_group *layers,
                                 struct hrt_view *view,
                                 struct hrt_output *output) {
    struct hrt_scene_fullscreen_node *new_node = calloc(1, sizeof(*new_node));

    if (!new_node) {
        wlr_log(WLR_ERROR, "Failed to allocate hrt_scene_fullscreen_node");
        return NULL;
    }

    struct wlr_scene_tree *root = wlr_scene_tree_create(layers->fullscreen);
    if (!root) {
        wlr_log(WLR_ERROR, "Failed to allocate wlr_scene_tree");
        return NULL;
    }
    int x, y;
    hrt_output_position(output, &x, &y);
    wlr_scene_node_set_position(&root->node, x, y);

    const float color[4] = {
        0,
        0,
        0,
        0,
    };
    int width, height;
    hrt_output_resolution(output, &width, &height);
    struct wlr_scene_rect *rect =
        wlr_scene_rect_create(root, width, height, color);
    if (!rect) {
        wlr_log(WLR_ERROR, "Failed to allocate hrt_scene_rect");
        return NULL;
    }

    new_node->tree       = root;
    new_node->background = rect;
    new_node->view       = view;
    root->node.data      = new_node;

    hrt_view_reparent(view, root);
    hrt_view_set_relative(view, 0, 0);

    return new_node;
}

struct hrt_view *
hrt_scene_fullscreen_node_destroy(struct hrt_scene_fullscreen_node *node) {
    struct hrt_view *view = node->view;
    hrt_view_reparent(view, node->background->node.parent->node.parent);
    // We don't need to free the background, as it's destroyed by
    // destroying its parent:
    wlr_scene_node_destroy(&node->tree->node);
    free(node);

    return view;
}

uint32_t hrt_scene_node_set_dimensions(struct hrt_scene_fullscreen_node *node,
                                       int width, int height) {
    wlr_scene_rect_set_size(node->background, width, height);
    return hrt_view_set_size(node->view, width, height);
}

void hrt_scene_node_set_position(struct hrt_scene_fullscreen_node *node, int x,
                                 int y) {
    wlr_scene_node_set_position(&node->tree->node, x, y);
}

uint32_t hrt_scene_fullscreen_configure(struct hrt_scene_fullscreen_node *node,
                                        struct hrt_output *output) {
    int x, y;
    hrt_output_position(output, &x, &y);
    wlr_scene_node_set_position(&node->tree->node, x, y);

    int width, height;
    hrt_output_resolution(output, &width, &height);
    return hrt_scene_node_set_dimensions(node, width, height);
}
