#ifndef HRT_OUTPUT_H
#define HRT_OUTPUT_H

#include <wayland-server.h>

#include <wlr/types/wlr_output.h>

#include <hrt_server.h>

struct hrt_output {
  struct wlr_output *wlr_output;
  struct hrt_server *server;
  struct wl_list link;

  struct wl_listener frame;
  struct wl_listener destroy;

  // temp background color
  float color[4];
};

bool hrt_output_init(struct hrt_server *server);
#endif
