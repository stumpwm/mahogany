#include <stdlib.h>

#include <hrt/hrt_scene.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_session_lock.h>

#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/types/wlr_session_lock_v1.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/util/log.h>

struct hrt_session_lock {
    struct wlr_scene_tree *tree;

    bool abandoned;
    struct {
        struct wl_listener new_surface;
        struct wl_listener unlock;
        struct wl_listener destroy;
    } events;
};

static void handle_new_surface(struct wl_listener *listener, void *data) {}

static void handle_unlock(struct wl_listener *listener, void *data) {}

static void handle_lock_abandon(struct wl_listener *listener, void *data) {}

static void hrt_session_lock_destroy(struct hrt_session_lock *lock) {
    wlr_scene_node_destroy(&lock->tree->node);

    wl_list_remove(&lock->events.destroy.link);
    wl_list_remove(&lock->events.new_surface.link);
    wl_list_remove(&lock->events.unlock.link);

    free(lock);
}

static struct hrt_session_lock *
hrt_session_lock_create(struct hrt_server *server,
                        struct wlr_session_lock_v1 *lock_request) {
    struct hrt_session_lock *lock = calloc(1, sizeof(*lock));
    if (!lock) {
        wlr_log(WLR_ERROR,
                "Failed to allocate hrt_session lock; not locking session.");
        wlr_session_lock_v1_destroy(lock_request);
        return nullptr;
    }

    lock->tree = wlr_scene_tree_create(server->scene_root->lock);
    if (!lock->tree) {
        free(lock);
        wlr_session_lock_v1_destroy(lock_request);
        return nullptr;
    }

    lock->events.unlock.notify = handle_unlock;
    wl_signal_add(&lock_request->events.unlock, &lock->events.unlock);
    lock->events.destroy.notify = handle_lock_abandon;
    wl_signal_add(&lock_request->events.destroy, &lock->events.destroy);
    lock->events.new_surface.notify = handle_new_surface;
    wl_signal_add(&lock_request->events.new_surface, &lock->events.new_surface);

    return lock;
}

static void handle_session_lock_manager_destroy(struct wl_listener *listener,
                                                void *data) {
    struct hrt_server *server = wl_container_of(
        listener, server, destroy_listener.session_lock_manager);

    if (server->session_lock) {
        hrt_session_lock_destroy(server->session_lock);
    }

    wl_list_remove(&server->destroy_listener.session_lock_manager.link);
    wl_list_remove(&server->session_lock_new.link);

    server->session_lock_manager = nullptr;
}

static void handle_session_lock_manager_new_lock(struct wl_listener *listener,
                                                 void *data) {
    struct wlr_session_lock_v1 *lock_request = data;
    struct hrt_server *server =
        wl_container_of(listener, server, session_lock_new);

    if (server->session_lock) {
        if (server->session_lock->abandoned) {
            hrt_session_lock_destroy(server->session_lock);
        } else {
            wlr_log(WLR_DEBUG,
                    "Rejecting session lock: session already locked");
            wlr_session_lock_v1_destroy(lock_request);
            return;
        }
    }
}

bool session_lock_manager_init(struct hrt_server *server) {
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
