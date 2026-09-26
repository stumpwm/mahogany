#include "wlr/util/log.h"
#include <hrt/hrt_server.h>
#include <hrt/hrt_session_lock.h>
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/types/wlr_session_lock_v1.h>

static void handle_session_lock_manager_destroy(struct wl_listener *listener,
                                                void *data) {
    struct hrt_server *server = wl_container_of(
        listener, server, destroy_listener.session_lock_manager);

    wl_list_remove(&server->destroy_listener.session_lock_manager.link);
}

static void handle_session_lock_manager_new_lock(struct wl_listener *listener,
                                                 void *data) {
    struct wlr_session_lock_v1 *lock_request = data;
    struct hrt_server *server =
        wl_container_of(listener, server, session_lock_new);
}

bool init_session_lock_manager(struct hrt_server *server) {
    struct wlr_session_lock_manager_v1 *manager =
        wlr_session_lock_manager_v1_create(server->wl_display);
    if (!manager) {
        wlr_log(WLR_ERROR, "Failed to allocate wlr_session_lock_manager_v1");
        return false;
    }
    server->session_lock_manager = manager;

    server->destroy_listener.session_lock_manager.notify =
        handle_session_lock_manager_destroy;
    wl_signal_add(&manager->events.destroy,
                  &server->destroy_listener.session_lock_manager);

    server->session_lock_new.notify = handle_session_lock_manager_new_lock;
    wl_signal_add(&manager->events.new_lock, &server->session_lock_new);

    return true;
}
