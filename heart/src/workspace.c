#include <wayland-util.h>
#include <wlr/types/wlr_ext_workspace_v1.h>

#include <workspace_impl.h>

static void handle_workspace_manager_destroy(struct wl_listener *listener,
                                             void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, destroy_listener.workspace_manager);

    wl_list_remove(&server->workspace_commit.link);
    wl_list_remove(&listener->link);
}

static void handle_workspace_commit(struct wl_listener *listener, void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, destroy_listener.workspace_manager);
    wl_list_remove(&listener->link);
}

bool hrt_workspace_init(struct hrt_server *server,
                        struct hrt_workspace_callbacks *callbacks) {
    server->workspace_manager =
        wlr_ext_workspace_manager_v1_create(server->wl_display, 1);
    if (!server->workspace_manager) {
        wlr_log(WLR_ERROR, "Failed to create wlr_ext_workspace_manager");
        return false;
    }

    server->destroy_listener.workspace_manager.notify =
        handle_workspace_manager_destroy;
    wl_signal_add(&server->workspace_manager->events.destroy,
                  &server->destroy_listener.workspace_manager);

    server->workspace_commit.notify = handle_workspace_commit;
    wl_signal_add(&server->workspace_manager->events.commit,
                  &server->workspace_commit);
    return true;
}
