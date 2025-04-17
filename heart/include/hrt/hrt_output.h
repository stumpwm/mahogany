#ifndef HRT_HRT_OUTPUT_H
#define HRT_HRT_OUTPUT_H

#include <wayland-server.h>

#include <wlr/types/wlr_output.h>

#include <hrt/hrt_server.h>

struct hrt_output {
    struct wlr_output *wlr_output;
    struct hrt_server *server;

    struct wl_listener request_state;
    struct wl_listener frame;
    struct wl_listener destroy;

    // temp background color
    float color[4];
};

struct hrt_output_callbacks {
    void (*output_added)(struct hrt_output *output);
    void (*output_removed)(struct hrt_output *output);
    void (*output_layout_changed)();
};

bool hrt_output_init(struct hrt_server *server,
                     const struct hrt_output_callbacks *callbacks);
void hrt_output_destroy(struct hrt_server *server);
/**
 * Get the effective output resolution of the output that can be used to
 * set the width and height of views.
 **/
void hrt_output_resolution(struct hrt_output *output, int *width, int *height);

void hrt_output_position(struct hrt_output *output, int *x, int *y);

char *hrt_output_name(struct hrt_output *output);

char *hrt_output_make(struct hrt_output *output);

char *hrt_output_model(struct hrt_output *output);

char *hrt_output_serial(struct hrt_output *output);

#endif
