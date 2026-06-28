#include "hrt/hrt_output.h"
#include "hrt/hrt_scene.h"
#include "hrt/hrt_view.h"
#include "wlr/util/log.h"
#include <assert.h>
#include <stdlib.h>
#include <wayland-server-core.h>
#include <wayland-util.h>

static void handle_scene_destroy(struct wl_listener *listener, void *data) {
    struct hrt_scene_root *root =
        wl_container_of(listener, root, listeners.scene_destroy);

    wl_list_remove(&root->listeners.scene_destroy.link);

    hrt_scene_root_destroy(root);
}

struct hrt_scene_root *
hrt_scene_root_create(struct wlr_scene_tree *scene_tree) {
    assert(scene_tree != NULL);
    struct hrt_scene_root *scene_root = calloc(1, sizeof(*scene_root));
    scene_root->background            = wlr_scene_tree_create(scene_tree);
    scene_root->bottom                = wlr_scene_tree_create(scene_tree);
    scene_root->normal                = wlr_scene_tree_create(scene_tree);
    scene_root->fullscreen            = wlr_scene_tree_create(scene_tree);
    scene_root->top                   = wlr_scene_tree_create(scene_tree);
    scene_root->overlay               = wlr_scene_tree_create(scene_tree);

    scene_root->listeners.scene_destroy.notify = handle_scene_destroy;
    wl_signal_add(&scene_tree->node.events.destroy,
                  &scene_root->listeners.scene_destroy);

    return scene_root;
}

void hrt_scene_root_destroy(struct hrt_scene_root *scene_root) {
    wlr_log(WLR_DEBUG, "Destroying scene root");
    wlr_scene_node_destroy(&scene_root->overlay->node);
    wlr_scene_node_destroy(&scene_root->top->node);
    wlr_scene_node_destroy(&scene_root->fullscreen->node);
    wlr_scene_node_destroy(&scene_root->normal->node);
    wlr_scene_node_destroy(&scene_root->bottom->node);
    wlr_scene_node_destroy(&scene_root->background->node);
    free(scene_root);
}

struct hrt_scene_output *hrt_scene_output_create(struct hrt_scene_root *scene) {
    struct hrt_scene_output *scene_output = calloc(1, sizeof(*scene_output));
    scene_output->background = wlr_scene_tree_create(scene->background);
    scene_output->bottom     = wlr_scene_tree_create(scene->bottom);
    scene_output->top        = wlr_scene_tree_create(scene->top);
    scene_output->overlay    = wlr_scene_tree_create(scene->overlay);
    return scene_output;
}

void hrt_scene_output_destroy(struct hrt_scene_output *output) {
    wlr_log(WLR_DEBUG, "Destroying scene output");
    wlr_scene_node_destroy(&output->overlay->node);
    wlr_scene_node_destroy(&output->top->node);
    wlr_scene_node_destroy(&output->bottom->node);
    wlr_scene_node_destroy(&output->background->node);
    free(output);
}

struct wlr_scene_tree *
hrt_scene_output_get_layer(struct hrt_scene_output *output,
                           enum zwlr_layer_shell_v1_layer layer_type) {
    switch (layer_type) {
        case ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND:
            return output->background;
        case ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM:
            return output->bottom;
        case ZWLR_LAYER_SHELL_V1_LAYER_TOP:
            return output->top;
        case ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY:
            return output->overlay;
        default:
          return NULL;
    }
}

struct hrt_scene_group *hrt_scene_group_create(struct hrt_scene_root *parent) {
    struct hrt_scene_group *group = calloc(1, sizeof(*group));
    if (!group) {
        wlr_log(WLR_ERROR, "Could not allocate hrt_scene_layers");
        return NULL;
    }
    group->layers      = wlr_scene_tree_create(parent->normal);
    if (!group->layers) {
        wlr_log(WLR_ERROR, "Could not create wlr_scene_tree for scene group");
        return nullptr;
    }

    return group;
}

void hrt_scene_group_destroy(struct hrt_scene_group *group) {
    wlr_log(WLR_DEBUG, "Destroying scene group");
    wlr_scene_node_destroy(&group->layers->node);
    free(group);
}

void hrt_scene_group_set_enabled(struct hrt_scene_group *group, bool enabled) {
    wlr_scene_node_set_enabled(&group->layers->node, enabled);
}

static void reparent_children(struct wlr_scene_tree *source,
                              struct wlr_scene_tree *dest) {
    struct wlr_scene_node *child, *child_tmp;
    wl_list_for_each_safe(child, child_tmp, &source->children, link) {
        wlr_scene_node_reparent(child, dest);
    }
}

void hrt_scene_layer_transfer(struct wlr_scene_tree *source,
                              struct wlr_scene_tree *destination) {
    reparent_children(source, destination);
}

struct wlr_scene_tree *hrt_scene_group_layers(struct hrt_scene_group *group) {
    return group->layers;
}

struct wlr_scene_tree *hrt_scene_layer_create(struct hrt_scene_group *group) {
  struct wlr_scene_tree *layer = wlr_scene_tree_create(group->layers);
    if (!layer) {
      wlr_log(WLR_ERROR, "Could not create scene tree for hrt_scene_layer (group %p)", group);
      return nullptr;
    }
    return layer;
}

void hrt_scene_layer_destroy(struct wlr_scene_tree *layer) {
    wlr_scene_node_destroy(&layer->node);
}

void hrt_scene_layer_add_view(struct wlr_scene_tree *layer,
                              struct hrt_view *view) {
    wlr_scene_node_reparent(&view->scene_tree->node, layer);
}

struct hrt_scene_fullscreen_node *
hrt_scene_create_fullscreen_node(struct wlr_scene_tree *layer,
                                 struct hrt_view *view,
                                 struct hrt_output *output) {
    struct hrt_scene_fullscreen_node *new_node = calloc(1, sizeof(*new_node));

    if (!new_node) {
        wlr_log(WLR_ERROR, "Failed to allocate hrt_scene_fullscreen_node");
        return NULL;
    }

    struct wlr_scene_tree *root = wlr_scene_tree_create(layer);
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

    new_node->layer = root;
    new_node->background = rect;
    new_node->view       = view;
    root->node.data      = new_node;

    hrt_view_reparent(view, root);
    hrt_view_set_relative(view, 0, 0);
    hrt_view_set_size(view, width, height);

    return new_node;
}

struct hrt_view *hrt_scene_fullscreen_swap(struct hrt_scene_fullscreen_node *node,
			       struct hrt_view *view) {
  struct hrt_view *v = node->view;
  struct wlr_scene_tree *p = view->scene_tree->node.parent;
  hrt_view_reparent(view, node->layer);
  hrt_view_reparent(node->view, p);
  node->view = view;
  hrt_view_fullscreen(view, true);
  return v;
}

struct hrt_view *
hrt_scene_fullscreen_node_destroy(struct hrt_scene_fullscreen_node *node) {
    struct hrt_view *view = node->view;
    hrt_view_reparent(view, node->background->node.parent->node.parent);
    // We don't need to free the background, as it's destroyed by
    // destroying its parent:
    wlr_scene_node_destroy(&node->layer->node);
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
    wlr_scene_node_set_position(&node->layer->node, x, y);
}

uint32_t hrt_scene_fullscreen_configure(struct hrt_scene_fullscreen_node *node,
                                        struct hrt_output *output) {
    int x, y;
    hrt_output_position(output, &x, &y);
    wlr_scene_node_set_position(&node->layer->node, x, y);

    int width, height;
    hrt_output_resolution(output, &width, &height);
    return hrt_scene_node_set_dimensions(node, width, height);
}
