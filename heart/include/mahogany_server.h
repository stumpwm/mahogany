#ifndef MAHOGANY_SERVER_H
#define MAHOGANY_SERVER_H

#include <stdbool.h>

#include <wayland-server.h>
#include <wlr/backend.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_output_management_v1.h>


struct mahogany_callbacks {

};

struct mahogany_server {
  struct wl_display *wl_display;
  struct wlr_backend *backend;
  struct wlr_renderer *renderer;
  struct wlr_compositor *compositor;

  struct wl_list outputs;
  struct wl_listener new_output;
  struct wlr_output_manager_v1 *output_manager;
  struct wlr_output_layout *output_layout;
  struct wl_listener output_manager_apply;
  struct wl_listener output_manager_test;
};

bool mahogany_server_init(struct mahogany_server *server);

bool mahogany_server_start(struct mahogany_server *server);

void mahogany_server_run(struct mahogany_server *server);

void mahogany_server_finish(struct mahogany_server *server);

#endif
