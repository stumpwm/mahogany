#include "hrt/hrt_layer_shell.h"
#include "hrt/hrt_scene.h"
#include "hrt/hrt_output.h"
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

    wlr_log(WLR_DEBUG, "layer %d", layer_type);
    surface->scene_layer =
        wlr_scene_layer_surface_v1_create(output_layer, surface->layer_surface);
    wlr_log(WLR_DEBUG, "Surface created");
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

static void handle_surface_commit(struct wl_listener *listener, void *data) {
    struct hrt_layer_shell_surface *surface =
        wl_container_of(listener, surface, events.commit);
    wlr_log(WLR_DEBUG, "layer shell surface commit");
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
    wl_signal_add(&surface->scene_layer->tree->node.events.destroy,
                  &surface->events.scene_destroy);
}

struct hrt_output *
hrt_layer_surface_output(struct hrt_layer_shell_surface *layer_shell) {
    return (struct hrt_output *)layer_shell->layer_surface->data;
}
