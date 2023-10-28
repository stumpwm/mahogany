#include "wlr/util/log.h"
#include "xdg_impl.h"
#include <stdlib.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_subcompositor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_xdg_shell.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>

bool hrt_server_init(struct hrt_server *server,
					 const struct hrt_output_callbacks *output_callbacks,
					 const struct hrt_seat_callbacks *seat_callbacks,
					 const struct hrt_view_callbacks *view_callbacks,
					 enum wlr_log_importance log_level) {
  wlr_log_init(log_level, NULL);
  server->wl_display = wl_display_create();
  server->backend = wlr_backend_autocreate(server->wl_display);

  if(!server->backend) {
    return false;
  }

  server->renderer = wlr_renderer_autocreate(server->backend);
  if(!server->renderer) {
    return false;
  }
  wlr_renderer_init_wl_display(server->renderer, server->wl_display);

  server->allocator = wlr_allocator_autocreate(server->backend, server->renderer);
  if(!server->allocator) {
    return false;
  }

  server->compositor = wlr_compositor_create(server->wl_display, server->renderer);
  wlr_subcompositor_create(server->wl_display);
  wlr_data_device_manager_create(server->wl_display);

  wlr_export_dmabuf_manager_v1_create(server->wl_display);
  wlr_screencopy_manager_v1_create(server->wl_display);
  wlr_data_control_manager_v1_create(server->wl_display);
  wlr_gamma_control_manager_v1_create(server->wl_display);

  server->output_layout = wlr_output_layout_create();

  server->view_callbacks = view_callbacks;

  server->xdg_shell = wlr_xdg_shell_create(server->wl_display, 3);
  server->new_xdg_surface.notify = handle_new_xdg_surface;
  wl_signal_add(&server->xdg_shell->events.new_surface, &server->new_xdg_surface);


  if(!hrt_output_init(server, output_callbacks)) {
    return false;
  }
  if(!hrt_seat_init(&server->seat, server, seat_callbacks)) {
    return false;
  }

  return true;
}

static char *prev_wayland_display;

bool hrt_server_start(struct hrt_server *server) {
  const char *socket = wl_display_add_socket_auto(server->wl_display);

  if(!socket) {
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

	if(prev_wayland_display) {
		setenv("WAYLAND_DISPLAY", prev_wayland_display, true);
	} else {
		unsetenv("WAYLAND_DISPLAY");
	}
}

void hrt_server_finish(struct hrt_server *server) {
  wl_display_destroy_clients(server->wl_display);
  wl_display_destroy(server->wl_display);
}

struct wlr_scene_tree *hrt_server_scene_tree(struct hrt_server *server) {
	return &server->scene->tree;
}
