#include "hrt/hrt_output.h"
#include "session_lock_impl.h"
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

    struct wl_list outputs; // hrt_session_lock_output

    struct {
        struct wl_listener new_surface;
        struct wl_listener unlock;
        struct wl_listener destroy;
        struct wl_listener scene_tree_destroy;
    } events;
};

struct hrt_session_lock_output {
    struct hrt_session_lock *lock;
    struct hrt_output *output;
    struct wlr_scene_rect *background;

    struct {
        struct wl_listener scene_tree_destroy;
    } events;

    struct wl_list link;
};

static void handle_new_surface(struct wl_listener *listener, void *data) {}

static void
session_lock_output_destroy(struct hrt_session_lock_output *lock_output) {
    wl_list_remove(&lock_output->link);

    if (lock_output->background) {
        wlr_scene_node_destroy(&lock_output->background->node);
    }

    free(lock_output);
}

static void session_lock_output_scene_tree_destroy(struct wl_listener *listener,
                                                   void *data) {
    struct hrt_session_lock_output *lock_output =
        wl_container_of(listener, lock_output, events.scene_tree_destroy);
    lock_output->background = nullptr;
    wl_list_remove(&lock_output->events.scene_tree_destroy.link);
}

static struct hrt_session_lock_output *
session_lock_output_create(struct hrt_session_lock *lock,
                           struct hrt_output *output) {
    struct hrt_session_lock_output *lock_output =
        calloc(1, sizeof(*lock_output));

    lock_output->output = output;
    lock_output->lock   = lock;

    int x, y, width, height;
    hrt_output_resolution(output, &width, &height);
    hrt_output_position(output, &x, &y);

    const float color[4] = {0, 1, 0, 1};
    lock_output->background =
        wlr_scene_rect_create(lock->tree, width, height, color);
    wlr_scene_node_set_position(&lock_output->background->node, x, y);

    lock_output->events.scene_tree_destroy.notify =
        session_lock_output_scene_tree_destroy;
    wl_signal_add(&lock_output->background->node.events.destroy, &lock_output->events.scene_tree_destroy);

    wl_list_insert(&lock->outputs, &lock_output->link);

    return lock_output;
}

static void
session_lock_output_place(struct hrt_session_lock_output *lock_output, struct hrt_output *output) {
    int x, y, width, height;
    hrt_output_resolution(output, &width, &height);
    hrt_output_position(output, &x, &y);

    wlr_scene_node_set_position(&lock_output->background->node, x, y);
    wlr_scene_rect_set_size(lock_output->background, width, height);
}

void session_lock_arrange(struct hrt_server *server) {
    if (!server->session_lock) {
        return;
    }
    struct hrt_session_lock const *lock = server->session_lock;
    struct hrt_session_lock_output *lock_output;
    wl_list_for_each(lock_output, &lock->outputs, link) {
        session_lock_output_place(lock_output, lock_output->output);
    }
}

void session_lock_output_arrange(struct hrt_server *server,
                                 struct hrt_output *output) {
    if (!server->session_lock) {
        return;
    }
    struct hrt_session_lock const *lock = server->session_lock;
    struct hrt_session_lock_output *lock_output;
    wl_list_for_each(lock_output, &lock->outputs, link) {
        if (lock_output->output == output) {
            session_lock_output_place(lock_output, output);
            break;
        }
    }
}

static void handle_lock_abandon(struct wl_listener *listener, void *data) {
    struct hrt_session_lock *lock = wl_container_of(listener, lock, events.destroy);
    wlr_log(WLR_DEBUG, "Lock abandoned");

    lock->abandoned = true;

    wl_list_remove(&lock->events.destroy.link);
    wl_list_remove(&lock->events.new_surface.link);
    wl_list_remove(&lock->events.unlock.link);
}

static void handle_lock_scene_tree_destroy(struct wl_listener *listener,
                                           void *data) {
    struct hrt_session_lock *lock =
        wl_container_of(listener, lock, events.scene_tree_destroy);
    lock->tree = nullptr;
    wl_list_remove(&lock->events.scene_tree_destroy.link);
}

static void hrt_session_lock_destroy(struct hrt_session_lock *lock) {
    struct hrt_session_lock_output *output, *tmp;
    wl_list_for_each_safe(output, tmp, &lock->outputs, link) {
        session_lock_output_destroy(output);
    }

    if(lock->tree) {
        wlr_scene_node_destroy(&lock->tree->node);
    }

    // If a lock is abandoned, we remove these already:
    if(!lock->abandoned) {
        wl_list_remove(&lock->events.destroy.link);
        wl_list_remove(&lock->events.new_surface.link);
        wl_list_remove(&lock->events.unlock.link);
    }

    free(lock);
}

static void handle_unlock(struct wl_listener *listener, void *data) {
    struct hrt_session_lock *lock =
        wl_container_of(listener, lock, events.unlock);

    hrt_session_lock_destroy(lock);
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
        wlr_log(WLR_ERROR,
                "Failed to allocate scene tree for hrt_session_lock; not "
                "locking session");
        free(lock);
        wlr_session_lock_v1_destroy(lock_request);
        return nullptr;
    }
    // Due to how the shutdown sequence works, if the server closes while there is a lock,
    // the scene tree gets destroyed before this object, so we need to clear out
    // our pointers to it when that happens:
    lock->events.scene_tree_destroy.notify = handle_lock_scene_tree_destroy;
    wl_signal_add(&lock->tree->node.events.destroy, &lock->events.scene_tree_destroy);

    lock->events.unlock.notify = handle_unlock;
    wl_signal_add(&lock_request->events.unlock, &lock->events.unlock);
    lock->events.destroy.notify = handle_lock_abandon;
    wl_signal_add(&lock_request->events.destroy, &lock->events.destroy);
    lock->events.new_surface.notify = handle_new_surface;
    wl_signal_add(&lock_request->events.new_surface, &lock->events.new_surface);

    wl_list_init(&lock->outputs);

    struct wlr_output_layout_output *output;
    wl_list_for_each(output, &server->output_layout->outputs, link) {
        struct wlr_output *wlr_output = output->output;
        struct hrt_output *hrt_output = wlr_output->data;

        session_lock_output_create(lock, hrt_output);
    }

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
            // Do we actually want to destroy this, or just
            // reassign the wlr_session_lock_v1 object?
            hrt_session_lock_destroy(server->session_lock);
        } else {
            wlr_log(WLR_DEBUG,
                    "Rejecting session lock: session already locked");
            wlr_session_lock_v1_destroy(lock_request);
            return;
        }
    }

    struct hrt_session_lock *lock =
        hrt_session_lock_create(server, lock_request);
    server->session_lock = lock;
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
