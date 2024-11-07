#include <assert.h>
#include <stdint.h>
#include <time.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

#include "hrt/hrt_input.h"
#include "hrt/hrt_view.h"
#include "wlr/util/log.h"

void hrt_view_init(struct hrt_view *view, struct wlr_scene_tree *tree) {
  assert(view->scene_tree == NULL && "View already initialized");
  view->scene_tree = wlr_scene_tree_create(tree);
  view->width = 0;
  view->height = 0;

	struct wlr_scene_tree *xdg_tree =
		wlr_scene_xdg_surface_create(view->scene_tree, view->xdg_toplevel->base);
	xdg_tree->node.data = view;
	view->xdg_surface->data = xdg_tree;
}

void hrt_view_cleanup(struct hrt_view *view) {
	if(view->xdg_surface->data) {
		wlr_scene_node_destroy(view->xdg_surface->data);
	}
	if(view->scene_tree) {
		wlr_scene_node_destroy(&view->scene_tree->node);
	}
}

uint32_t hrt_view_set_size(struct hrt_view *view, int width, int height) {
  view->width = width;
  view->height = height;
  if(view->xdg_surface->initialized) {
	  return wlr_xdg_toplevel_set_size(view->xdg_toplevel, width, height);
  }
  return 0;
}

void hrt_view_set_relative(struct hrt_view *view, int x, int y) {
	wlr_scene_node_set_position(&view->scene_tree->node, x, y);
}

void hrt_view_focus(struct hrt_view *view, struct hrt_seat *seat) {
  wlr_log(WLR_DEBUG, "view focused!");
  struct wlr_seat *wlr_seat = seat->seat;
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
    wlr_seat_keyboard_notify_enter(wlr_seat, toplevel->base->surface,
                                   keyboard->keycodes, keyboard->num_keycodes,
                                   &keyboard->modifiers);
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
