#include "hrt/hrt_layer_shell.h"
#include "hrt/hrt_scene.h"
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/types/wlr_layer_shell_v1.h>

#include <hrt/hrt_server.h>

static void handle_new_layer_surface(struct wl_listener *listener, void *data) {
    struct wlr_layer_surface_v1 *layer_surface = data;
    struct hrt_server *server =
        wl_container_of(listener, server, new_layer_shell);

    struct hrt_layer_shell_surface *surface =
        hrt_layer_shell_surface_create(layer_surface);

    server->layer_shell_callbacks->new_surface(surface);

    /* if (!layer_surface->output) { */
    /*     // TODO: figure out which output is the current one, somehow: */

    /*     // for now, just abort: */
    /*     wlr_layer_surface_v1_destroy(layer_surface); */
    /*     return; */
    /* } */

    // Next, we need to figure out which layer the surface is going into
    // and preliminarily assign it there:
    //
    // layer_surface->pending.layer

    // Finally, hook up all of the event listeners the surface needs:
}

void hrt_layer_shell_surface_place(struct hrt_layer_shell_surface *surface,
                                   struct hrt_scene_output *output) {
    enum zwlr_layer_shell_v1_layer layer_type =
        surface->layer_surface->pending.layer;
    struct wlr_scene_tree *output_layer =
        hrt_scene_output_get_layer(output, layer_type);
    surface->scene_layer =
        wlr_scene_layer_surface_v1_create(output_layer, surface->layer_surface);

}

void hrt_layer_shell_surface_set_output(
    struct hrt_layer_shell_surface *layer_shell, struct hrt_output *output) {
    layer_shell->layer_surface->output = output->wlr_output;
}

bool hrt_layer_shell_init(struct hrt_server *server) {
    // I have little idea what the version means, this is what sway is using:
    struct wlr_layer_shell_v1 *shell =
        wlr_layer_shell_v1_create(server->wl_display, 5);
    if (!shell) {
        return false;
    }
    server->layer_shell = shell;

    server->new_layer_shell.notify = handle_new_layer_surface;
    wl_signal_add(&shell->events.new_surface, &server->new_layer_shell);

    return true;
}
