#ifndef HRT_HRT_OUTPUT_H
#define HRT_HRT_OUTPUT_H

#include "hrt/hrt_scene.h"
#include <wayland-server.h>

#include <wlr/types/wlr_output.h>
#include <wlr/util/box.h>

#include <hrt/hrt_server.h>

struct hrt_scene_output;

struct hrt_output_config {
    double scale;
    // Use the exact settings instead of the closest supported match:
    bool custom_mode;
    int width, height;
    float refresh_rate;
    // Place the output in a specific spot in the layout based on the
    // given X,Y coordinates:
    bool custom_position;
    // These are in layout coordinates, not pixel coordinates:
    int x, y;
    bool enabled;
};

struct hrt_output {
    struct wlr_output *wlr_output;
    struct hrt_server *server;
    struct hrt_scene_output *scene;
    struct wlr_scene_output *wlr_scene;

    struct wlr_box usable_area;

    struct wl_list layer_surfaces;

    struct wl_listener request_state;
    struct wl_listener frame;
    struct wl_listener destroy;
};

struct hrt_output_callbacks {
    void (*output_added)(struct hrt_output *output);
    void (*output_removed)(struct hrt_output *output);
    void (*output_layout_changed)();
};

bool hrt_output_configure(struct hrt_output *output,
                          struct hrt_output_config *config);

bool hrt_output_configure_atomic(struct hrt_output *outputs[],
                                 struct hrt_output_config configs[],
                                 const size_t length);

/**
 * Get the effective output resolution of the output that can be used to
 * set the width and height of views.
 **/
void hrt_output_resolution(struct hrt_output *output, int *width, int *height);

void hrt_output_position(struct hrt_output *output, int *x, int *y);

bool hrt_output_enabled(struct hrt_output *output);

char *hrt_output_name(struct hrt_output *output);

char *hrt_output_make(struct hrt_output *output);

char *hrt_output_model(struct hrt_output *output);

char *hrt_output_serial(struct hrt_output *output);

#endif
