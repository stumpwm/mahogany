#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>

bool hrt_server_init(struct hrt_server *server, const struct hrt_output_callbacks *output_callbacks,
		     const struct hrt_seat_callbacks *seat_callbacks) {
  server->wl_display = wl_display_create();
  server->backend = wlr_backend_autocreate(server->wl_display, NULL);

  if(!server->backend) {
    return false;
  }

  server->renderer = wlr_backend_get_renderer(server->backend);
  wlr_renderer_init_wl_display(server->renderer, server->wl_display);

  server->compositor = wlr_compositor_create(server->wl_display, server->renderer);

  wlr_export_dmabuf_manager_v1_create(server->wl_display);
  wlr_screencopy_manager_v1_create(server->wl_display);
  wlr_data_control_manager_v1_create(server->wl_display);
  wlr_gamma_control_manager_v1_create(server->wl_display);

  if(!hrt_output_init(server, output_callbacks)) {
    return false;
  }
  if(!hrt_seat_init(&server->seat, server, seat_callbacks)) {
    return false;
  }

  return true;
}

bool hrt_server_start(struct hrt_server *server) {
  if (!wlr_backend_start(server->backend)) {
    wlr_backend_destroy(server->backend);
    return false;
  }
  return true;
}

void hrt_server_run(struct hrt_server *server) {
  wl_display_run(server->wl_display);
}

void hrt_server_finish(struct hrt_server *server) {
  wl_display_destroy_clients(server->wl_display);
  wl_display_destroy(server->wl_display);
}
