#ifndef MAHOGANY_OUTPUT_H
#define MAHOGANY_OUTPUT_H

#include <wayland-server.h>

#include <wlr/types/wlr_output.h>

#include <mahogany_server.h>

struct mahogany_output {
  struct wlr_output *wlr_output;
  struct mahogany_server *server;
  struct wl_list link;

  struct wl_listener frame;
  struct wl_listener destroy;

  // temp background color
  float color[4];
};

bool output_init(struct mahogany_server *server);
#endif
