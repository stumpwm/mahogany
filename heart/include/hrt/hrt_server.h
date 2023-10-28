#ifndef HRT_HRT_SERVER_H
#define HRT_HRT_SERVER_H

#include <stdbool.h>

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
  struct wlr_renderer *renderer;
  struct wlr_compositor *compositor;
  struct wlr_allocator *allocator;

  struct wlr_scene *scene;
  struct wl_list outputs;
  struct wl_listener new_output;
  struct wlr_output_manager_v1 *output_manager;
  struct wlr_output_layout *output_layout;
  struct wl_listener output_manager_apply;
  struct wl_listener output_manager_test;

  struct hrt_seat seat;

  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;

  const struct hrt_output_callbacks *output_callback;
  const struct hrt_view_callbacks *view_callbacks;
};

bool hrt_server_init(struct hrt_server *server,
					 const struct hrt_output_callbacks *output_callbacks,
					 const struct hrt_seat_callbacks *seat_callbacks,
					 const struct hrt_view_callbacks *view_callbacks,
					 enum wlr_log_importance log_level);

bool hrt_server_start(struct hrt_server *server);

void hrt_server_stop(struct hrt_server *server);

void hrt_server_finish(struct hrt_server *server);

struct wlr_scene_tree *hrt_server_scene_tree(struct hrt_server *server);

#endif
