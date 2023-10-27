#include <stdint.h>
#include <wayland-server-core.h>
#include "hrt/hrt_server.h"

struct hrt_view {
	struct wlr_xdg_surface *xdg_surface;
	struct wlr_xdg_toplevel *xdg_toplevel;
	/*
	  Contains the tree with the xdg surface tree
	  plus decorations and that sort of thing.
	 */
	struct wlr_scene_tree *scene_tree;
	struct wl_listener map;
	struct wl_listener unmap;
	struct wl_listener destroy;
};

/**
 * Fully initialize the view and place it in the given scene tree.
 **/
void hrt_view_init(struct hrt_view *view, struct wlr_scene_tree *tree);

/**
 * Request that this view be the given size. Returns the associated configure serial.
 **/
uint32_t hrt_view_set_size(struct hrt_view *view, int width, int height);

/**
 * Sets the view to the given coordinates relative to its parent.
 **/
void hrt_view_set_relative(struct hrt_view *view, int x, int y);
