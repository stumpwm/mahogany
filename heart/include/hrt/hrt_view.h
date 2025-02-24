#ifndef HRT_VIEW
#define HRT_VIEW

#include <stdint.h>
#include <wayland-server-core.h>
#include <hrt/hrt_server.h>
#include "hrt_input.h"

struct hrt_view;

typedef void (*view_destroy_handler)(struct hrt_view *view);
typedef void (*new_view_handler)(struct hrt_view *view);

struct hrt_view {
	int width, height;
	struct wlr_xdg_surface *xdg_surface;
	struct wlr_xdg_toplevel *xdg_toplevel;
	/*
	  Contains the tree with the xdg surface tree
	  plus decorations and that sort of thing.
	 */
	struct wlr_scene_tree *scene_tree;

	// internal state:
	struct wl_listener map;
	struct wl_listener unmap;
	struct wl_listener commit;
	struct wl_listener destroy;

	struct wl_listener request_maximize;
	struct wl_listener request_fullscreen;

	new_view_handler new_view_handler;
	view_destroy_handler destroy_handler;
};

struct hrt_view_callbacks {
	/**
	 * A new view has been created. Must call `hrt_view_init` for the
	 * view to be displayed.
	 **/
	new_view_handler new_view;
	view_destroy_handler view_destroyed;
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

/**
 * Focus the given view and perform the needed tasks to make
 * it visible to the user.
 **/
void hrt_view_focus(struct hrt_view *view, struct hrt_seat *seat);

/**
 * Unfocus the given view.
 **/
void hrt_view_unfocus(struct hrt_view *view, struct hrt_seat *seat);

/**
 * Stop the given view from being displayed
 **/
void hrt_view_set_hidden(struct hrt_view *view, bool hidden);

void hrt_view_reparent(struct hrt_view *view, struct wlr_scene_tree *node);

/**
 * Request that the view be closed. This is the "nice" version
 * that is the same as clicking the close button on window decorations.
 * It does not garentee that the application actually closes, but
 * well behaved ones should.
 **/
void hrt_view_request_close(struct hrt_view *view);

#endif
