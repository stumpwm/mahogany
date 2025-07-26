#ifndef HRT_HRT_SERVER_H
#define HRT_HRT_SERVER_H

#include "hrt/hrt_scene.h"
#include "hrt/hrt_output.h"
#include "hrt/hrt_layer_shell.h"
#include "wlr/backend/session.h"
#include <stdbool.h>

#include <wayland-server-core.h>
#include <wayland-server.h>
#include <wlr/backend.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_output_management_v1.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/render/allocator.h>
#include <wlr/util/log.h>

#include <hrt/hrt_input.h>

struct hrt_server {
    struct wl_display *wl_display;
    struct wlr_backend *backend;
    struct wlr_backend *headless_backend;

    struct wlr_session *session;
    struct wlr_renderer *renderer;
    struct wlr_compositor *compositor;
    struct wlr_allocator *allocator;

    struct wlr_scene *scene;
    struct wlr_scene_output_layout *scene_layout;
    struct wl_listener new_output;
    struct wlr_output_manager_v1 *output_manager;
    struct wlr_output_layout *output_layout;
    struct wl_listener output_layout_changed;
    struct wl_listener output_manager_apply;
    struct wl_listener output_manager_test;

    struct hrt_seat seat;
    struct hrt_scene_root *scene_root;
    struct hrt_output *fallback_output;

    struct wlr_xdg_shell *xdg_shell;
    struct wl_listener new_xdg_toplevel;
    struct wl_listener new_xdg_popup;

    struct wlr_layer_shell_v1 *layer_shell;
    struct wl_listener new_layer_shell;

    struct {
        struct wl_listener backend;
        struct wl_listener headless;
        struct wl_listener output_manager;
        struct wl_listener layer_shell;
    } destroy_listener;

    const struct hrt_output_callbacks *output_callback;
    const struct hrt_view_callbacks *view_callbacks;
    const struct hrt_layer_shell_callbacks *layer_shell_callbacks;

    struct wl_event_source *message_timer_source;
    struct wlr_scene_buffer *message_buffer;
};

bool hrt_server_init(
    struct hrt_server *server,
    const struct hrt_output_callbacks *output_callbacks,
    const struct hrt_seat_callbacks *seat_callbacks,
    const struct hrt_view_callbacks *view_callbacks,
    const struct hrt_layer_shell_callbacks *layer_shell_callbacks,
    enum wlr_log_importance log_level);

bool hrt_server_start(struct hrt_server *server);

void hrt_server_stop(struct hrt_server *server);

void hrt_server_finish(struct hrt_server *server);

struct wlr_scene_tree *hrt_server_scene_tree(struct hrt_server *server);

struct hrt_seat *hrt_server_seat(struct hrt_server *server);

struct hrt_scene_group *hrt_server_group_create(struct hrt_server *server);

size_t hrt_server_struct_size();

#endif
