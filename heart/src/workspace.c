#include "hrt/hrt_workspace.h"
#include "wlr/util/log.h"
#include <stdint.h>
#include <wayland-protocols/ext-workspace-v1-enum.h>
#include <wayland-util.h>
#include <wlr/types/wlr_ext_workspace_v1.h>

static void handle_workspace_manager_destroy(struct wl_listener *listener,
                                             void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, destroy_listener.workspace_manager);

    wl_list_remove(&server->workspace_commit.link);
    wl_list_remove(&listener->link);
}

static void handle_workspace_commit(struct wl_listener *listener, void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, workspace_commit);
    struct wlr_ext_workspace_v1_commit_event *event = data;
    const struct hrt_workspace_callbacks *callbacks =
        server->workspace_callbacks;

    struct wlr_ext_workspace_v1_request *request;
    wl_list_for_each(request, event->requests, link) {
        // I don't know if wlroots blocks requests that we don't advertise capabilities for,
        // so check to be on the safe side:
        switch (request->type) {
            case WLR_EXT_WORKSPACE_V1_REQUEST_CREATE_WORKSPACE:
                if (callbacks->create_workspace) {
                    callbacks->create_workspace(
                        request->create_workspace.name,
                        request->create_workspace.group);
                }
                break;
            case WLR_EXT_WORKSPACE_V1_REQUEST_ACTIVATE:
                if (callbacks->activate_workspace) {
                    callbacks->activate_workspace(request->activate.workspace);
                }
                break;
            case WLR_EXT_WORKSPACE_V1_REQUEST_DEACTIVATE:
                if (callbacks->decativate_workspace) {
                    callbacks->decativate_workspace(
                        request->deactivate.workspace);
                }
                break;
            case WLR_EXT_WORKSPACE_V1_REQUEST_ASSIGN:
                if (callbacks->assign_workspace) {
                    callbacks->assign_workspace(request->assign.workspace,
                                                request->assign.group);
                }
                break;
            case WLR_EXT_WORKSPACE_V1_REQUEST_REMOVE:
                if (callbacks->remove_workspace) {
                    callbacks->remove_workspace(request->remove.workspace);
                }
                break;
        }
    }
}

bool hrt_workspace_init(struct hrt_server *server,
                        struct hrt_workspace_callbacks *callbacks) {
    if (server->workspace_manager) {
        wlr_log(WLR_ERROR, "ext_workspace_manager already initialized");
        return false;
    }

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

hrt_workspace_group_handle *
hrt_workspace_group_handle_create(struct hrt_server *server) {
    const struct hrt_workspace_callbacks *callbacks =
        server->workspace_callbacks;
    uint32_t caps = 0;
    if (callbacks->create_workspace) {
        caps |=
            EXT_WORKSPACE_GROUP_HANDLE_V1_GROUP_CAPABILITIES_CREATE_WORKSPACE;
    }
    return wlr_ext_workspace_group_handle_v1_create(server->workspace_manager,
                                                    caps);
}

void hrt_workspace_group_handle_destroy(hrt_workspace_group_handle *group) {
    wlr_ext_workspace_group_handle_v1_destroy(group);
}

void hrt_workspace_group_add_output(hrt_workspace_group_handle *group,
                                    struct hrt_output *output) {
    wlr_ext_workspace_group_handle_v1_output_enter(group, output->wlr_output);
}

void hrt_workspace_group_handle_output_leave(hrt_workspace_group_handle *group,
                                             struct hrt_output *output) {
    wlr_ext_workspace_group_handle_v1_output_leave(group, output->wlr_output);
}

hrt_workspace_handle *hrt_workspace_handle_create(struct hrt_server *server,
                                                  const char *id) {
    const struct hrt_workspace_callbacks *callbacks =
        server->workspace_callbacks;
    uint32_t caps = 0;
    if (callbacks->activate_workspace) {
        caps |= EXT_WORKSPACE_HANDLE_V1_WORKSPACE_CAPABILITIES_ACTIVATE;
    }
    if (callbacks->decativate_workspace) {
        caps |= EXT_WORKSPACE_HANDLE_V1_WORKSPACE_CAPABILITIES_DEACTIVATE;
    }
    if (callbacks->remove_workspace) {
        caps |= EXT_WORKSPACE_HANDLE_V1_WORKSPACE_CAPABILITIES_REMOVE;
    }
    if (callbacks->assign_workspace) {
        caps |= EXT_WORKSPACE_HANDLE_V1_WORKSPACE_CAPABILITIES_ASSIGN;
    }
    return wlr_ext_workspace_handle_v1_create(server->workspace_manager, id,
                                              caps);
}

void hrt_workspace_handle_destroy(hrt_workspace_handle *workspace) {
    wlr_ext_workspace_handle_v1_destroy(workspace);
}

void hrt_workspace_handle_set_group(hrt_workspace_handle *workspace,
                                    hrt_workspace_group_handle *group) {
    wlr_ext_workspace_handle_v1_set_group(workspace, group);
}

void hrt_workspace_handle_set_name(hrt_workspace_handle *workspace,
                                   const char *name) {
    wlr_ext_workspace_handle_v1_set_name(workspace, name);
}

void hrt_workspace_handle_set_coordinates(hrt_workspace_handle *workspace,
                                          const uint32_t *coords,
                                          size_t coords_len) {
    wlr_ext_workspace_handle_v1_set_coordinates(workspace, coords, coords_len);
}

void hrt_workspace_handle_set_active(hrt_workspace_handle *workspace,
                                     bool enabled) {
    wlr_ext_workspace_handle_v1_set_active(workspace, enabled);
}

void hrt_workspace_handle_set_urgent(hrt_workspace_handle *workspace,
                                     bool enabled) {
    wlr_ext_workspace_handle_v1_set_urgent(workspace, enabled);
}

void hrt_workspace_handle_set_hidden(hrt_workspace_handle *workspace,
                                     bool enabled) {
    wlr_ext_workspace_handle_v1_set_hidden(workspace, enabled);
}
