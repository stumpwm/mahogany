#ifndef HRT_LAYER_SHELL
#define HRT_LAYER_SHELL

#include <wayland-server-core.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>

struct hrt_output;

struct hrt_layer_shell_surface {
    struct wlr_layer_surface_v1 *layer_surface;
    struct wlr_scene_layer_surface_v1 *scene_layer;
    struct {
        struct wl_listener commit;
        struct wl_listener map;
        struct wl_listener unmap;
        struct wl_listener new_popup;
        struct wl_listener destroy;
    } events;
};

typedef void (*layer_shell_event_handler)(struct hrt_layer_shell_surface *);

struct hrt_layer_shell_callbacks {
    layer_shell_event_handler new_surface;
};

struct hrt_layer_shell_surface *
hrt_layer_shell_surface_create(struct wlr_layer_surface_v1 *surface);

struct hrt_output *
hrt_layer_surface_output(struct hrt_layer_shell_surface *layer_shell) {
    return (struct hrt_output *)layer_shell->layer_surface->data;
}

void hrt_layer_shell_surface_set_output(
    struct hrt_layer_shell_surface *layer_shell, struct hrt_output *output);

#endif
