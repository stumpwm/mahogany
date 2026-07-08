#include <assert.h>
#include <stdint.h>
#include <time.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

#include "hrt/hrt_input.h"
#include "hrt/hrt_view.h"
#include "seat_impl.h"
#include "wlr/util/log.h"

void hrt_view_info(struct hrt_view *view) {
    wlr_log(WLR_DEBUG, "New view: %s", view->xdg_toplevel->app_id);
}

void hrt_view_init(struct hrt_view *view, struct wlr_scene_tree *tree) {
    assert(view->scene_tree == NULL && "View already initialized");
    view->scene_tree = wlr_scene_tree_create(tree);
    view->width      = 0;
    view->height     = 0;

    struct wlr_scene_tree *xdg_tree = wlr_scene_xdg_surface_create(
        view->scene_tree, view->xdg_toplevel->base
    );
    xdg_tree->node.data     = view;
    view->xdg_surface->data = xdg_tree;

    view->xdg_scene = xdg_tree;

    hrt_view_info(view);
}

void hrt_view_cleanup(struct hrt_view *view) {
    if (view->xdg_surface->data) {
        // TODO: There's no obvious documentation
        //  on if the xdg_scene_tree gets cleaned up automatically.
        //  If it does, this might cause problems:
        wlr_scene_node_destroy(view->xdg_surface->data);
    }
    if (view->scene_tree) {
        wlr_scene_node_destroy(&view->scene_tree->node);
    }
}

uint32_t hrt_view_set_size(struct hrt_view *view, int width, int height) {
    view->width  = width;
    view->height = height;
    if (view->xdg_surface->initialized) {
        return wlr_xdg_toplevel_set_size(view->xdg_toplevel, width, height);
    }
    return 0;
}

bool hrt_view_mapped(struct hrt_view *view) {
    if (view->xdg_surface && view->xdg_surface->surface) {
        return view->xdg_surface->surface->mapped;
    } else {
        return false;
    }
}

void hrt_view_set_relative(struct hrt_view *view, int x, int y) {
    wlr_scene_node_set_position(&view->scene_tree->node, x, y);
}

void hrt_view_focus(struct hrt_view *view, struct hrt_seat *seat) {
    wlr_log(WLR_DEBUG, "view focused!");

    struct wlr_xdg_toplevel *toplevel = view->xdg_toplevel;
    if (hrt_seat_keyboard_focus_surface(seat, toplevel->base->surface)) {
        wlr_xdg_toplevel_set_activated(toplevel, true);
    }
}

void hrt_view_unfocus(struct hrt_view *view, struct hrt_seat *seat) {
    struct wlr_xdg_toplevel *toplevel = view->xdg_toplevel;
    if (hrt_seat_keyboard_focus_surface_clear(seat, toplevel->base->surface)) {
        wlr_xdg_toplevel_set_activated(toplevel, false);
    }
}

bool hrt_view_focused(struct hrt_view *view) {
    // FIXME: Should we be checking the pending or current state here?
    return view->xdg_toplevel->current.activated;
}

void hrt_view_set_hidden(struct hrt_view *view, bool hidden) {
    wlr_scene_node_set_enabled(&view->scene_tree->node, !hidden);
}

void hrt_view_reparent(struct hrt_view *view, struct wlr_scene_tree *node) {
    wlr_scene_node_reparent(&view->scene_tree->node, node);
}

void hrt_view_request_close(struct hrt_view *view) {
    wlr_xdg_toplevel_send_close(view->xdg_toplevel);
}

void hrt_view_send_configure(struct hrt_view *view) {
    if (view->xdg_toplevel->base->initialized) {
        wlr_xdg_surface_schedule_configure(view->xdg_toplevel->base);
    }
}

uint32_t hrt_view_fullscreen(struct hrt_view *view, bool fullscreen) {
    return wlr_xdg_toplevel_set_fullscreen(view->xdg_toplevel, fullscreen);
}

bool hrt_view_fullscreened(struct hrt_view *view) {
    return view->xdg_toplevel->current.fullscreen;
}
