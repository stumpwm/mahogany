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

	view->destroy_handler(view);

	wl_list_remove(&view->map.link);
	wl_list_remove(&view->unmap.link);
	wl_list_remove(&view->destroy.link);
	wl_list_remove(&view->commit.link);

	free(view);
}

static void handle_xdg_toplevel_commit(struct wl_listener *listener,
                                       void *data) {
	struct hrt_view *view = wl_container_of(listener, view, commit);
	if (view->xdg_toplevel->base->initial_commit) {
		wlr_xdg_toplevel_set_size(view->xdg_toplevel, view->width,view->height);
	}
}

static struct hrt_view *create_view_from_xdg_surface(struct wlr_xdg_toplevel *xdg_toplevel, view_destroy_handler destroy_handler) {
	struct hrt_view *view = calloc(1, sizeof(struct hrt_view));
	view->xdg_toplevel = xdg_toplevel;
	struct wlr_xdg_surface *xdg_surface = xdg_toplevel->base;
	// TODO: Maybe remove view->xdg_surface? We can get to it via the toplevel.
	view->xdg_surface = xdg_surface;
	view->destroy_handler = destroy_handler;

	view->map.notify = handle_xdg_toplevel_map;
	wl_signal_add(&xdg_surface->surface->events.map, &view->map);
	view->unmap.notify = handle_xdg_toplevel_unmap;
	wl_signal_add(&xdg_surface->surface->events.unmap, &view->unmap);
	view->destroy.notify = handle_xdg_toplevel_destroy;
	wl_signal_add(&xdg_surface->events.destroy, &view->destroy);
	view->commit.notify = handle_xdg_toplevel_commit;
	wl_signal_add(&xdg_toplevel->base->surface->events.commit, &view->commit);
	// TODO: We need to listen to the commit event so we can send the configure message on first commit

	return view;
}

static void handle_xdg_popup_commit(struct wl_listener *listener, void *data) {
	struct hrt_xdg_popup *popup = wl_container_of(listener, popup, commit);
	if (popup->xdg_popup->base->initial_commit) {
		wlr_xdg_surface_schedule_configure(popup->xdg_popup->base);
	}
}

static void handle_xdg_popup_destroy(struct wl_listener *listener, void *data) {
	struct hrt_xdg_popup *popup = wl_container_of(listener, popup, destroy);

	wl_list_remove(&popup->destroy.link);
	wl_list_remove(&popup->commit.link);

	free(popup);
}

static void handle_new_xdg_popup(struct wl_listener *listener, void *data) {
  wlr_log(WLR_DEBUG, "New xdg popup received");
  struct hrt_server *server = wl_container_of(listener, server, new_xdg_popup);
  struct wlr_xdg_popup *xdg_popup = data;

  // The front end doesn't need to know about popups; wlroots handles it for us.
  // we do need to set some internal data so that they can be rendered though.
  struct wlr_xdg_surface *parent = wlr_xdg_surface_try_from_wlr_surface(xdg_popup->parent);
  struct wlr_scene_tree *parent_tree = parent->data;

  // The parent view might not have been initizlized properly. In that case, it
  // isn't being displayed, so we just ignore it:
  if (parent_tree) {
	  xdg_popup->base->data = wlr_scene_xdg_surface_create(parent_tree, xdg_popup->base);
	  struct hrt_xdg_popup *popup = calloc(1, sizeof(*popup));
	  popup->commit.notify = handle_xdg_popup_commit;
	  wl_signal_add(&xdg_popup->base->surface->events.commit,
					&popup->commit);

	  popup->destroy.notify = handle_xdg_popup_destroy;
	  wl_signal_add(&xdg_popup->events.destroy, &popup->destroy);

  } else {
	  wlr_log(WLR_ERROR, "Encountered XDG Popup without properly configured parent");
  }
}

static void handle_new_xdg_toplevel(struct wl_listener *listener, void * data) {
	wlr_log(WLR_DEBUG, "New XDG Popup received");
	struct hrt_server *server =
		wl_container_of(listener, server, new_xdg_toplevel);
	struct wlr_xdg_toplevel *toplevel = data;
  	// Initialization occurs in two steps so the consumer can place the view where it needs to go;
	// in order to create a scene tree node, it must have a parent.
	// We don't have it until the callback.
	struct hrt_view *view = create_view_from_xdg_surface(toplevel,
														 server->view_callbacks->view_destroyed);
	// At some point, we will want the front end to call this, as it should decide what node
	// of the scene graph the view gets added to:
	// hrt_view_init(view, &server->scene->tree);

	server->view_callbacks->new_view(view);
}

bool hrt_xdg_shell_init(struct hrt_server *server) {
  server->xdg_shell = wlr_xdg_shell_create(server->wl_display, 3);
  server->new_xdg_popup.notify = handle_new_xdg_popup;
  wl_signal_add(&server->xdg_shell->events.new_popup, &server->new_xdg_popup);

  server->new_xdg_toplevel.notify = handle_new_xdg_toplevel;
  wl_signal_add(&server->xdg_shell->events.new_toplevel, &server->new_xdg_toplevel);
  return true;
}
