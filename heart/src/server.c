#include "hrt/hrt_scene.h"
#include "wlr/util/log.h"
#include "xdg_impl.h"
#include "seat_impl.h"
#include "output_impl.h"
#include "scene_impl.h"
#include "message_impl.h"
#include <stdlib.h>
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_viewporter.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>

static void handle_backend_destroyed(struct wl_listener *listener, void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, backend_destroy);
    wl_display_terminate(server->wl_display);

    wl_list_remove(&listener->link);
}

bool hrt_server_init(struct hrt_server *server,
                     const struct hrt_output_callbacks *output_callbacks,
                     const struct hrt_seat_callbacks *seat_callbacks,
                     const struct hrt_view_callbacks *view_callbacks,
                     enum wlr_log_importance log_level) {
    wlr_log_init(log_level, NULL);
    server->wl_display = wl_display_create();
    server->backend    = wlr_backend_autocreate(
        wl_display_get_event_loop(server->wl_display), &server->session);

    server->backend_destroy.notify = &handle_backend_destroyed;
    wl_signal_add(&server->backend->events.destroy, &server->backend_destroy);

    if (!server->backend) {
        return false;
    }

    server->renderer = wlr_renderer_autocreate(server->backend);
    if (!server->renderer) {
        return false;
    }
    wlr_renderer_init_wl_display(server->renderer, server->wl_display);

    server->allocator =
        wlr_allocator_autocreate(server->backend, server->renderer);
    if (!server->allocator) {
        return false;
    }

    server->compositor =
        wlr_compositor_create(server->wl_display, 5, server->renderer);
    wlr_subcompositor_create(server->wl_display);
    wlr_data_device_manager_create(server->wl_display);
    wlr_viewporter_create(server->wl_display);

    wlr_export_dmabuf_manager_v1_create(server->wl_display);
    wlr_screencopy_manager_v1_create(server->wl_display);
    wlr_data_control_manager_v1_create(server->wl_display);
    wlr_gamma_control_manager_v1_create(server->wl_display);
    wlr_primary_selection_v1_device_manager_create(server->wl_display);

    server->scene = wlr_scene_create();
    server->output_layout = wlr_output_layout_create(server->wl_display);
    server->scene_root = hrt_scene_root_create(&server->scene->tree);

    server->view_callbacks = view_callbacks;

    if (!hrt_xdg_shell_init(server)) {
        return false;
    }

    if (!hrt_output_init(server, output_callbacks)) {
        return false;
    }
    if (!hrt_seat_init(&server->seat, server, seat_callbacks)) {
        return false;
    }

    if (!hrt_message_init(server)) {
        return false;
    }

    return true;
}

static char *prev_wayland_display;

bool hrt_server_start(struct hrt_server *server) {
    const char *socket = wl_display_add_socket_auto(server->wl_display);

    if (!socket) {
        goto cleanup;
    }

    if (!wlr_backend_start(server->backend)) {
        goto cleanup;
    }

    prev_wayland_display = getenv("WAYLAND_DISPLAY");
    setenv("WAYLAND_DISPLAY", socket, true);
    wlr_log(WLR_INFO, "Running on Wayland socket: %s", socket);

    wl_display_run(server->wl_display);
    return true;

cleanup:
    wlr_backend_destroy(server->backend);
    return false;
}

void hrt_server_stop(struct hrt_server *server) {
    wl_display_terminate(server->wl_display);

    if (prev_wayland_display) {
        setenv("WAYLAND_DISPLAY", prev_wayland_display, true);
    } else {
        unsetenv("WAYLAND_DISPLAY");
    }
}

void hrt_server_finish(struct hrt_server *server) {
    wl_display_destroy_clients(server->wl_display);
    hrt_message_destroy(server);
    // Some of these "destroy" calls should probably be hooked up to listen to the destroy
    // events of the wlr objects they attach listeners to instead of being cleaned
    // up here...
    hrt_xdg_shell_destroy(server);
    hrt_seat_destroy(&server->seat);

    hrt_output_destroy(server);
    wlr_allocator_destroy(server->allocator);
    wlr_renderer_destroy(server->renderer);
    wlr_backend_destroy(server->backend);
    wl_display_destroy(server->wl_display);
    wlr_scene_node_destroy(&server->scene->tree.node);
}

struct wlr_scene_tree *hrt_server_scene_tree(struct hrt_server *server) {
    return &server->scene->tree;
}

struct hrt_seat *hrt_server_seat(struct hrt_server *server) {
    return &server->seat;
}

struct hrt_scene_group *hrt_server_group_create(struct hrt_server *server) {
  return hrt_scene_group_create(server->scene_root);
}

size_t hrt_server_struct_size() {
  return sizeof(struct hrt_server);
}
