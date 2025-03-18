#include "layer_shell_impl.h"

#include <wayland-util.h>
#include <wlr/types/wlr_layer_shell_v1.h>

static void handle_new_layer_surface(struct wl_listener *listener, void *data) {
    struct wlr_layer_surface_v1 *layer_surface = data;
    struct hrt_server *server =
        wl_container_of(listener, server, layer_shell_surface);
    if (!layer_surface->output) {
        // TODO: figure out which output is the current one, somehow:

        // for now, just abort:
        wlr_layer_surface_v1_destroy(layer_surface);
        return;
    }

    // Next, we need to figure out which layer the surface is going into
    // and preliminarily assign it there:
    //
    // layer_surface->pending.layer

    // Finally, hook up all of the event listeners the surface needs:
}

bool hrt_layer_shell_init(struct hrt_server *server) {
    // I have little idea what the version means, this is what sway is using:
    struct wlr_layer_shell_v1 *shell =
        wlr_layer_shell_v1_create(server->wl_display, 5);
    if (!shell) {
        return false;
    }
    server->layer_shell = shell;

    server->layer_shell_surface.notify = handle_new_layer_surface;
    wl_signal_add(&shell->events.new_surface, &server->layer_shell_surface);

    return true;
}
