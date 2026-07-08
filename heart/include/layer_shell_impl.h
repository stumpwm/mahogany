#ifndef LAYER_SHELL_IMPL
#define LAYER_SHELL_IMPL

#include "hrt/hrt_server.h"
#include "hrt/hrt_layer_shell.h"
#include <wayland-server-core.h>

struct hrt_layer_shell_popup {
    struct wlr_xdg_popup *wlr_popup;
    struct wlr_scene_tree *scene;
    struct hrt_layer_shell_surface *toplevel;

    struct wl_listener destroy;
    struct wl_listener new_popup;
    struct wl_listener commit;
    struct wl_listener reposition;
};

bool hrt_layer_shell_init(
    struct hrt_server *server, const struct hrt_layer_shell_callbacks *callbacks
);

void hrt_layer_shell_arrange_layers(struct hrt_output *output, bool emit_event);

#endif
