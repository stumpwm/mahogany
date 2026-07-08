#include <assert.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/util/log.h>

#include "hrt/hrt_input.h"
#include "hrt/hrt_output.h"
#include "hrt/hrt_scene.h"
#include "hrt/hrt_server.h"
#include "xdg_impl.h"
#include "hrt/hrt_view.h"
#include "view_impl.h"

#include <wlr/types/wlr_xdg_shell.h>

static void handle_xdg_toplevel_map(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "XDG Toplevel Mapped!");
    struct hrt_view *view = wl_container_of(listener, view, map);
    // The callback should be sending a configure event:
    view->callbacks->view_mapped(view);
}

static void
handle_xdg_toplevel_unmap(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "XDG Toplevel unmapped!");
    struct hrt_view *view = wl_container_of(listener, view, unmap);
    view->callbacks->view_unmapped(view);
}

static void
handle_xdg_toplevel_request_maximize(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "XDG Toplevel request maximize");
    struct hrt_view *view = wl_container_of(listener, view, request_maximize);
    // The protocol specifies that after this request is made, we must
    // send a configure event.
    if (view->callbacks->request_maximize) {
        view->callbacks->request_maximize(view);
    } else {
        hrt_view_send_configure(view);
    }
}

static void
handle_xdg_toplevel_request_minimize(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "XDG Toplevel request maximize");
    struct hrt_view *view = wl_container_of(listener, view, request_minimize);
    // The protocol specifies that after this request is made, we must
    // send a configure event.
    if (view->callbacks->request_minimize) {
        view->callbacks->request_minimize(view);
    } else {
        hrt_view_send_configure(view);
    }
}

static void handle_xdg_toplevel_request_fullscreen(
    struct wl_listener *listener, void *data
) {
    struct hrt_view *view = wl_container_of(listener, view, request_fullscreen);
    struct wlr_xdg_toplevel *toplevel = view->xdg_toplevel;

    if (!toplevel->base->surface->mapped) {
        hrt_view_send_configure(view);
        return;
    }

    struct hrt_output *requested_output    = NULL;
    struct wlr_xdg_toplevel_requested *req = &toplevel->requested;
    if (req->fullscreen && req->fullscreen_output &&
        req->fullscreen_output->data) {
        requested_output = req->fullscreen_output->data;
    }
    bool changed = view->callbacks->request_fullscreen(
        view, requested_output, req->fullscreen
    );
    wlr_log(WLR_DEBUG, "Fullscreen request fufilled: %d", changed);
    // If no change was made, we still need to send a configure event;
    // send a blank one to show that nothing happened:
    if (!changed) {
        hrt_view_send_configure(view);
    }
}

static void
handle_xdg_toplevel_destroy(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "XDG Toplevel Destroyed!");
    struct hrt_view *view = wl_container_of(listener, view, destroy);

    view->callbacks->view_destroyed(view);

    wl_list_remove(&view->map.link);
    wl_list_remove(&view->unmap.link);
    wl_list_remove(&view->destroy.link);
    wl_list_remove(&view->commit.link);
    wl_list_remove(&view->request_fullscreen.link);
    wl_list_remove(&view->request_maximize.link);
    wl_list_remove(&view->request_minimize.link);
    wl_list_remove(&view->new_popup.link);

    hrt_view_cleanup(view);
    free(view);
}

static void
handle_xdg_toplevel_commit(struct wl_listener *listener, void *data) {
    struct hrt_view *view = wl_container_of(listener, view, commit);
    struct wlr_xdg_toplevel *const toplevel = view->xdg_toplevel;
    if (toplevel->base->initial_commit) {
        view->callbacks->new_view(view);
    } else if (view->xdg_surface->surface->mapped) {
        const uint32_t committed = toplevel->base->current.committed;
        if (committed & WLR_XDG_SURFACE_STATE_WINDOW_GEOMETRY) {
            view->callbacks->view_size_changed(view);
        }
    }
}

static void handle_new_xdg_popup(struct wl_listener *listener, void *data);

static struct hrt_view *create_view_from_xdg_surface(
    struct wlr_xdg_toplevel *xdg_toplevel, struct hrt_server *server
) {
    struct hrt_view *view = calloc(1, sizeof(struct hrt_view));
    if (!view) {
        wlr_log(
            WLR_ERROR,
            "Failed to allocate hrt_view object for toplevel %p",
            xdg_toplevel
        );
        return nullptr;
    }
    view->xdg_toplevel                  = xdg_toplevel;
    struct wlr_xdg_surface *xdg_surface = xdg_toplevel->base;
    // TODO: Maybe remove view->xdg_surface? We can get to it via the toplevel.
    view->xdg_surface = xdg_surface;
    view->callbacks   = server->view_callbacks;

    // Using the scene root here is not as efficient as it could be,
    // as we then need to reparent the node in the initial commit.
    // Depending on how many nodes are in the tree,
    // it could be costly. It may also cause flickering, but I haven't
    // observed that.
    hrt_view_init(view, server->scene_root->normal);

    view->map.notify = handle_xdg_toplevel_map;
    wl_signal_add(&xdg_surface->surface->events.map, &view->map);
    view->unmap.notify = handle_xdg_toplevel_unmap;
    wl_signal_add(&xdg_surface->surface->events.unmap, &view->unmap);
    view->destroy.notify = handle_xdg_toplevel_destroy;
    wl_signal_add(&xdg_toplevel->events.destroy, &view->destroy);
    view->commit.notify = handle_xdg_toplevel_commit;
    wl_signal_add(&xdg_toplevel->base->surface->events.commit, &view->commit);
    view->new_popup.notify = handle_new_xdg_popup;
    wl_signal_add(&xdg_surface->events.new_popup, &view->new_popup);

    // Swaywm registers these when the toplevel is mapped, but I don't think  that should make
    // a difference:
    view->request_fullscreen.notify = handle_xdg_toplevel_request_fullscreen;
    wl_signal_add(
        &xdg_toplevel->events.request_fullscreen, &view->request_fullscreen
    );
    view->request_maximize.notify = &handle_xdg_toplevel_request_maximize;
    wl_signal_add(
        &xdg_toplevel->events.request_maximize, &view->request_maximize
    );
    view->request_minimize.notify = &handle_xdg_toplevel_request_minimize;
    wl_signal_add(
        &xdg_toplevel->events.request_minimize, &view->request_minimize
    );

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
    wl_list_remove(&popup->new_popup.link);

    free(popup);
}

static void
handle_popup_new_xdg_popup(struct wl_listener *listener, void *data);

static struct hrt_xdg_popup *create_popup(
    struct hrt_view *view, struct wlr_xdg_popup *xdg_popup,
    struct wlr_scene_tree *parent
) {
    struct hrt_xdg_popup *popup = calloc(1, sizeof(*popup));
    if (!popup) {
        wlr_log(WLR_ERROR, "Failed to allocated hrt_xdg_popup");
        return nullptr;
    }
    popup->view      = view;
    popup->xdg_popup = xdg_popup;
    popup->scene     = wlr_scene_xdg_surface_create(parent, xdg_popup->base);
    xdg_popup->base->data = popup->scene;

    popup->commit.notify = handle_xdg_popup_commit;
    wl_signal_add(&xdg_popup->base->surface->events.commit, &popup->commit);

    popup->destroy.notify = handle_xdg_popup_destroy;
    wl_signal_add(&xdg_popup->events.destroy, &popup->destroy);

    popup->new_popup.notify = handle_popup_new_xdg_popup;
    wl_signal_add(&xdg_popup->base->events.new_popup, &popup->new_popup);

    return popup;
}

static void
handle_popup_new_xdg_popup(struct wl_listener *listener, void *data) {
    struct hrt_xdg_popup *parent = wl_container_of(listener, parent, new_popup);
    struct wlr_xdg_popup *xdg_popup = data;
    create_popup(parent->view, xdg_popup, parent->scene);
}

static void handle_new_xdg_popup(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "New xdg popup received");
    struct hrt_view *view = wl_container_of(listener, view, new_popup);
    struct wlr_xdg_popup *xdg_popup = data;
    create_popup(view, xdg_popup, view->xdg_scene);
}

static void handle_new_xdg_toplevel(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "New XDG Toplevel received");
    struct hrt_server *server =
        wl_container_of(listener, server, new_xdg_toplevel);
    struct wlr_xdg_toplevel *toplevel = data;
    // Initialization occurs in two steps so the consumer can place the view
    // where it needs to go; in order to create a scene tree node, it must have
    // a parent. We don't have it until the callback.
    create_view_from_xdg_surface(toplevel, server);
}

static void check_callbacks(const struct hrt_view_callbacks *callbacks) {
    assert(callbacks->new_view != nullptr);
    assert(callbacks->view_size_changed != nullptr);
    assert(callbacks->view_mapped != nullptr);
    assert(callbacks->view_unmapped != nullptr);
    assert(callbacks->request_minimize != nullptr);
    assert(callbacks->request_maximize != nullptr);
    assert(callbacks->view_destroyed != nullptr);
    assert(callbacks->request_fullscreen != nullptr);
}

bool hrt_xdg_shell_init(struct hrt_server *server) {
    check_callbacks(server->view_callbacks);
    server->xdg_shell = wlr_xdg_shell_create(server->wl_display, 3);
    if (!server->xdg_shell) {
        wlr_log(WLR_ERROR, "Could not initialize wlr_xdg_shell");
        return false;
    }

    server->new_xdg_toplevel.notify = handle_new_xdg_toplevel;
    wl_signal_add(
        &server->xdg_shell->events.new_toplevel, &server->new_xdg_toplevel
    );
    return true;
}

void hrt_xdg_shell_destroy(struct hrt_server *server) {
    wl_list_remove(&server->new_xdg_toplevel.link);
}
