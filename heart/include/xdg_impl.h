#pragma once

#include <wayland-server-core.h>
#include <wayland-server.h>
#include "hrt/hrt_server.h"
#include "hrt/hrt_view.h"

struct hrt_xdg_popup {
    struct wlr_xdg_popup *xdg_popup;
    struct wlr_scene_tree *scene;
    struct hrt_view *view;

    struct wl_listener new_popup;
    struct wl_listener destroy;
    struct wl_listener commit;
};

bool hrt_xdg_shell_init(struct hrt_server *server);
void hrt_xdg_shell_destroy(struct hrt_server *server);
