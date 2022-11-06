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

void handle_new_xdg_surface(struct wl_listener *listener, void *data) {
	wlr_log(WLR_DEBUG, "New XDG Surface recieved");
	struct hrt_server *server = wl_container_of(listener, server, new_xdg_surface);
	struct wlr_xdg_surface *xdg_surface = data;

	if(xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
		return;
	}

	struct hrt_view *view = calloc(1, sizeof(struct hrt_view));

	view->map.notify = handle_xdg_toplevel_map;
	wl_signal_add(&xdg_surface->events.map, &view->map);
	view->unmap.notify = handle_xdg_toplevel_unmap;
	wl_signal_add(&xdg_surface->events.unmap, &view->unmap);
	view->destroy.notify = handle_xdg_toplevel_destroy;
	wl_signal_add(&xdg_surface->events.destroy, &view->destroy);
}
