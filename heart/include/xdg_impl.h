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
    struct wl_listener reposition;
};

/**
 * Keeps the popup inside the output. lx, ly is the layout top-left corner
 * of the window or layer surface the popup belongs to.
 * For a popup of a popup wlroots adds the parent offsets, so we use the same lx, ly.
 */
void hrt_popup_unconstrain(struct wlr_xdg_popup *popup,
                           struct hrt_output *output, int lx, int ly);

/**
 * Move the popups of the views to where their views are, and hide them along
 * with their views.
 * Call it whenever a view, or a tree it is in, is moved or hidden.
 */
void hrt_xdg_popups_arrange(struct hrt_server *server);

bool hrt_xdg_shell_init(struct hrt_server *server);
void hrt_xdg_shell_destroy(struct hrt_server *server);
