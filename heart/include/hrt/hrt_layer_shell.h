#ifndef HRT_LAYER_SHELL
#define HRT_LAYER_SHELL

#include "hrt_scene.h"
#include <wayland-server-core.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>

#include <hrt/hrt_scene.h>

struct hrt_output;

struct hrt_layer_shell_surface {
    struct wlr_layer_surface_v1 *layer_surface;
    struct wlr_scene_layer_surface_v1 *scene_surface;
    struct hrt_output *output;
    struct wlr_scene_tree *tree;
    bool mapped;
    struct {
        struct wl_listener commit;
        struct wl_listener map;
        struct wl_listener unmap;
        struct wl_listener new_popup;
        struct wl_listener scene_destroy;
    } events;
};

typedef void (*layer_shell_event_handler)(struct hrt_layer_shell_surface *);

struct hrt_layer_shell_callbacks {
    layer_shell_event_handler new_layer_surface;
};

struct hrt_layer_shell_surface *
hrt_layer_shell_surface_create(struct wlr_layer_surface_v1 *surface);

/**
 * Destroy a partially created hrt_layer_shell-surface object
 */
void hrt_layer_shell_surface_abort(struct hrt_layer_shell_surface *surface);

struct hrt_output *
hrt_layer_surface_output(struct hrt_layer_shell_surface *layer_shell);

void hrt_layer_shell_surface_set_output(
    struct hrt_layer_shell_surface *layer_shell, struct hrt_output *output);

/**
 * Place a freshly-initialized surface in an output. Should only be called once during
 * intial placement.
 */
void hrt_layer_shell_surface_place(struct hrt_layer_shell_surface *surface,
                                   struct hrt_output *output);

/**
 * Finish initializing the layer shell object
 */
void hrt_layer_shell_finish_init(struct hrt_layer_shell_surface *surface);

#endif
