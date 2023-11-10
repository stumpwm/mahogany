#ifndef HRT_HRT_OUTPUT_H
#define HRT_HRT_OUTPUT_H

#include <wayland-server.h>

#include <wlr/types/wlr_output.h>

#include <hrt/hrt_server.h>

struct hrt_output {
  struct wlr_output *wlr_output;
  struct hrt_server *server;
  void (*mode_change_handler)(struct hrt_output *output);

  struct wl_listener frame;
  struct wl_listener destroy;
  struct wl_listener mode;

  // temp background color
  float color[4];
};

struct hrt_output_callbacks {
  void (*output_added)(struct hrt_output *output);
  void (*output_removed)(struct hrt_output *output);
  void (*output_mode_changed)(struct hrt_output *output);
};

bool hrt_output_init(struct hrt_server *server, const struct hrt_output_callbacks *callbacks);

/**
 * Get the effective output resolution of the output that can be used to
 * set the width and height of views.
 **/
void hrt_output_resolution(struct hrt_output *output, int *width, int *height);
#endif
