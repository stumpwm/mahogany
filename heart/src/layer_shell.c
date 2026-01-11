#include "hrt/hrt_layer_shell.h"
#include "hrt/hrt_output.h"
#include "hrt/hrt_scene.h"
#include "wlr/util/box.h"
#include "wlr/util/log.h"

#include <assert.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/types/wlr_layer_shell_v1.h>

#include <hrt/hrt_server.h>

struct hrt_layer_shell_surface *
hrt_layer_shell_surface_create(struct wlr_layer_surface_v1 *surface) {
    struct hrt_layer_shell_surface *shell_surface =
        calloc(1, sizeof(struct hrt_layer_shell_surface));

    shell_surface->layer_surface = surface;
    return shell_surface;
}

void hrt_layer_shell_surface_abort(struct hrt_layer_shell_surface *surface) {
    wlr_layer_surface_v1_destroy(surface->layer_surface);
    free(surface);
}

static void handle_new_layer_surface(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "New layer surface");
    struct wlr_layer_surface_v1 *layer_surface = data;
    struct hrt_server *server =
        wl_container_of(listener, server, new_layer_shell);
    struct hrt_layer_shell_surface *surface =
        hrt_layer_shell_surface_create(layer_surface);

    // The rest of the initialization should happen in the callback:
    server->layer_shell_callbacks->new_layer_surface(surface);
}

void hrt_layer_shell_surface_place(struct hrt_layer_shell_surface *surface,
                                   struct hrt_scene_output *output) {
    enum zwlr_layer_shell_v1_layer layer_type =
        surface->layer_surface->pending.layer;
    struct wlr_scene_tree *output_layer =
        hrt_scene_output_get_layer(output, layer_type);
    assert(output_layer);

    wlr_log(WLR_DEBUG, "placing in layer %d", layer_type);
    surface->scene_surface =
        wlr_scene_layer_surface_v1_create(output_layer, surface->layer_surface);
    // Set this so we can reference it in the arrange_surface function
    // FIXME: This pointer is used for other things; it may be good to
    // try wlr_addons to store this data instead.
    surface->scene_surface->tree->node.data = surface;
    wlr_log(WLR_DEBUG, "Surface created");
    surface->output = output;
}

void hrt_layer_shell_surface_set_output(
    struct hrt_layer_shell_surface *layer_shell, struct hrt_output *output) {
    layer_shell->layer_surface->output = output->wlr_output;
}

static void hrt_layer_shell_destroy(struct wl_listener *listener, void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, destroy_listener.layer_shell);

    wl_list_remove(&server->new_layer_shell.link);
    wl_list_remove(&server->destroy_listener.layer_shell.link);
}

bool hrt_layer_shell_init(struct hrt_server *server) {
    struct wlr_layer_shell_v1 *shell =
        wlr_layer_shell_v1_create(server->wl_display, 5);
    if (!shell) {
        return false;
    }
    server->layer_shell = shell;

    server->new_layer_shell.notify = handle_new_layer_surface;
    wl_signal_add(&shell->events.new_surface, &server->new_layer_shell);
    server->destroy_listener.layer_shell.notify = hrt_layer_shell_destroy;
    wl_signal_add(&shell->events.destroy,
                  &server->destroy_listener.layer_shell);

    return true;
}

static void arrange_surface(const struct wlr_box *full_area,
                            struct wlr_box *usable_area,
                            struct wlr_scene_tree *tree, bool exclusive) {
    struct wlr_scene_node *node;
    wl_list_for_each(node, &tree->children, link) {
        // this should work assuming that the only children of the given tree are
        // from layer shell objects:
        struct hrt_layer_shell_surface *surface = node->data;
        // surface could be null during destruction
        if (!surface) {
            wlr_log(WLR_DEBUG, "No surface");
            continue;
        }

        if (!surface->layer_surface->initialized) {
            wlr_log(WLR_DEBUG, "Not initailzed");
            continue;
        }

        if ((surface->layer_surface->current.exclusive_zone > 0) != exclusive) {
            wlr_log(WLR_DEBUG, "exclusive");
            continue;
        }

        wlr_log(WLR_DEBUG, "Sending configure");
        wlr_scene_layer_surface_v1_configure(surface->scene_surface, full_area,
                                             usable_area);
    }
}

static void arrange_layers(struct hrt_output *output,
                           struct hrt_scene_output *scene_output) {
    struct wlr_box usable_area = {0};
    hrt_output_position(output, &usable_area.x, &usable_area.y);
    wlr_output_effective_resolution(output->wlr_output, &usable_area.width,
                                    &usable_area.height);
    const struct wlr_box full_area = usable_area;

    arrange_surface(&full_area, &usable_area, scene_output->overlay, true);
    arrange_surface(&full_area, &usable_area, scene_output->top, true);
    arrange_surface(&full_area, &usable_area, scene_output->bottom, true);
    arrange_surface(&full_area, &usable_area, scene_output->background, true);

    arrange_surface(&full_area, &usable_area, scene_output->overlay, false);
    arrange_surface(&full_area, &usable_area, scene_output->top, false);
    arrange_surface(&full_area, &usable_area, scene_output->bottom, false);
    arrange_surface(&full_area, &usable_area, scene_output->background, false);

    if (!wlr_box_equal(&usable_area, &output->usable_area)) {
        wlr_log(WLR_DEBUG, "Usable area changed, rearranging output");
        output->usable_area = usable_area;
        // TODO: this reconfigures all outputs, we can do way less work:
        output->server->output_callback->output_layout_changed();
    } else {
        // arrange_popups(root->layers.popup);
    }
}

static void handle_surface_commit(struct wl_listener *listener, void *data) {
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.commit);

    struct wlr_layer_surface_v1 *layer_surface = surface->layer_surface;
    uint32_t committed = layer_surface->current.committed;
    if (layer_surface->initialized &&
        committed & WLR_LAYER_SURFACE_V1_STATE_LAYER) {
        enum zwlr_layer_shell_v1_layer layer_type =
            layer_surface->current.layer;
        struct wlr_scene_tree *output_layer =
            hrt_scene_output_get_layer(surface->output, layer_type);
        wlr_scene_node_reparent(&surface->scene_surface->tree->node,
                                output_layer);
    }

    if (layer_surface->initial_commit || committed ||
        layer_surface->surface->mapped != surface->mapped) {
        wlr_log(WLR_DEBUG, "layer shell surface initial commit");
        surface->mapped               = layer_surface->surface->mapped;
        struct wlr_output *output     = surface->layer_surface->output;
        struct hrt_output *hrt_output = output->data;
        arrange_layers(hrt_output, surface->output);
    }
}

static void handle_map(struct wl_listener *listener, void *data) {
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.map);
    wlr_log(WLR_DEBUG, "layer shell surface mapped");
}

static void handle_unmap(struct wl_listener *listener, void *data) {
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.unmap);
    wlr_log(WLR_DEBUG, "layer shell surface unmapped");
}

static void handle_new_popup(struct wl_listener *listener, void *data) {
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.new_popup);
    wlr_log(WLR_DEBUG, "layer shell surface popup");
}

static void handle_node_destroy(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "layer shell surface node destroy");
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.scene_destroy);

    surface->scene_surface->tree->node.data = NULL;
    if (surface->output) {
        struct wlr_output *output     = surface->layer_surface->output;
        struct hrt_output *hrt_output = output->data;
        arrange_layers(hrt_output, surface->output);
    }

    wl_list_remove(&surface->events.scene_destroy.link);
    wl_list_remove(&surface->events.new_popup.link);
    wl_list_remove(&surface->events.unmap.link);
    wl_list_remove(&surface->events.map.link);
    wl_list_remove(&surface->events.commit.link);

    free(surface);
}

void hrt_layer_shell_finish_init(struct hrt_layer_shell_surface *surface) {
    struct wlr_layer_surface_v1 *const layer_surface = surface->layer_surface;
    surface->events.commit.notify                    = handle_surface_commit;
    wl_signal_add(&layer_surface->surface->events.commit,
                  &surface->events.commit);
    surface->events.map.notify = handle_map;
    wl_signal_add(&layer_surface->surface->events.map, &surface->events.map);
    surface->events.unmap.notify = handle_unmap;
    wl_signal_add(&layer_surface->surface->events.unmap,
                  &surface->events.unmap);
    surface->events.new_popup.notify = handle_new_popup;
    wl_signal_add(&layer_surface->events.new_popup, &surface->events.new_popup);

    surface->events.scene_destroy.notify = handle_node_destroy;
    wl_signal_add(&surface->scene_surface->tree->node.events.destroy,
                  &surface->events.scene_destroy);
}

struct hrt_output *
hrt_layer_surface_output(struct hrt_layer_shell_surface *layer_shell) {
    return (struct hrt_output *)layer_shell->layer_surface->data;
}
