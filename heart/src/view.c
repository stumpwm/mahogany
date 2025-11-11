#include <assert.h>
#include <stdint.h>
#include <time.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

#include "hrt/hrt_input.h"
#include "hrt/hrt_view.h"
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
        view->scene_tree, view->xdg_toplevel->base);
    xdg_tree->node.data     = view;
    view->xdg_surface->data = xdg_tree;
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
  if(view->xdg_surface && view->xdg_surface->surface) {
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
    struct wlr_seat *wlr_seat        = seat->seat;
    struct wlr_surface *prev_surface = wlr_seat->keyboard_state.focused_surface;
    struct wlr_xdg_toplevel *toplevel = view->xdg_toplevel;

    if (prev_surface == toplevel->base->surface) {
        // Don't re-focus an already focused surface:
        return;
    }
    struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(wlr_seat);

    wlr_xdg_toplevel_set_activated(toplevel, true);

    if (keyboard != NULL) {
        wlr_log(WLR_DEBUG, "Keyboard enter!");
        wlr_seat_keyboard_notify_enter(
            wlr_seat, toplevel->base->surface, keyboard->keycodes,
            keyboard->num_keycodes, &keyboard->modifiers);
    }
}

void hrt_view_unfocus(struct hrt_view *view, struct hrt_seat *seat) {
    struct wlr_xdg_toplevel *toplevel = view->xdg_toplevel;
    wlr_xdg_toplevel_set_activated(toplevel, false);
    wlr_seat_keyboard_notify_clear_focus(seat->seat);
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
