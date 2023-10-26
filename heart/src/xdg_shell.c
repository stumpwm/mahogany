#include <assert.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/util/log.h>

#include "hrt/hrt_input.h"
#include "xdg_impl.h"
#include "hrt/hrt_view.h"

#include <wlr/types/wlr_xdg_shell.h>

static void handle_xdg_toplevel_map(struct wl_listener *listener, void *data) {
	wlr_log(WLR_DEBUG, "XDG Toplevel Mapped!");
}

static void handle_xdg_toplevel_unmap(struct wl_listener *listener,
                                      void *data) {
	wlr_log(WLR_DEBUG, "XDG Toplevel unmapped!");
}

static void handle_xdg_toplevel_destroy(struct wl_listener *listener,
                                        void *data) {
	wlr_log(WLR_DEBUG, "XDG Toplevel Destroyed!");
	struct hrt_view *view = wl_container_of(listener, view, destroy);

	wl_list_remove(&view->map.link);
	wl_list_remove(&view->unmap.link);
	wl_list_remove(&view->destroy.link);

	free(view);
}

struct hrt_view *initialize_view(struct wlr_xdg_surface *xdg_surface, struct wlr_scene_tree *tree) {
	// This method can only deal with toplevel xdg_surfaces:
	assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);
	struct hrt_view *view = calloc(1, sizeof(struct hrt_view));
	view->xdg_toplevel = xdg_surface->toplevel;

	// Add the view to the scene tree (we should probab
	view->scene_tree = wlr_scene_xdg_surface_create(tree, view->xdg_toplevel->base);
	view->scene_tree->node.data = view;
	xdg_surface->data = view->scene_tree;

	// Listen to events:
	view->map.notify = handle_xdg_toplevel_map;
	wl_signal_add(&xdg_surface->events.map, &view->map);
	view->unmap.notify = handle_xdg_toplevel_unmap;
	wl_signal_add(&xdg_surface->events.unmap, &view->unmap);
	view->destroy.notify = handle_xdg_toplevel_destroy;
	wl_signal_add(&xdg_surface->events.destroy, &view->destroy);

	return view;
}


void handle_new_xdg_surface(struct wl_listener *listener, void *data) {
	wlr_log(WLR_DEBUG, "New XDG Surface recieved");
	struct hrt_server *server = wl_container_of(listener, server, new_xdg_surface);
	struct wlr_xdg_surface *xdg_surface = data;

	if(xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
		// We the front end doesn't need to know about popups; wlroots handles it for us.
		// we do need to set some internal data so that they can be rendered though.
		struct wlr_xdg_surface *parent = wlr_xdg_surface_from_wlr_surface(xdg_surface->popup->parent);
		struct wlr_scene_tree *parent_tree = parent->data;
		xdg_surface->data = wlr_scene_xdg_surface_create(
			parent_tree, xdg_surface);
		return;
	}

	// At some point, we will want the front end to call this, as it should decide what node
	// of the scene graph the view gets added to:
	initialize_view(xdg_surface, &server->scene->tree);
}
