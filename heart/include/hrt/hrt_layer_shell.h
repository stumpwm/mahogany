#ifndef HRT_LAYER_SHELL
#define HRT_LAYER_SHELL

#include "hrt_scene.h"
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include <wayland-server-core.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>

#include <hrt/hrt_scene.h>
#include <hrt/hrt_input.h>

struct hrt_output;
struct hrt_layer_shell_callbacks;

struct hrt_layer_shell_surface {
    struct wlr_layer_surface_v1 *layer_surface;
    struct wlr_scene_layer_surface_v1 *scene_surface;
    struct hrt_output *output;
    struct wlr_scene_tree *tree;
    bool mapped;
    const struct hrt_layer_shell_callbacks *callbacks;
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
    layer_shell_event_handler layer_surface_mapped;
    layer_shell_event_handler layer_surface_unmapped;
    void (*layers_reconfigured)(struct hrt_output *);
    layer_shell_event_handler keyboard_interactivity_updated;
    layer_shell_event_handler layer_changed;
};

enum hrt_layer_shell_keyboard_interactivity {
    HRT_LAYER_SHELL_KEYBOARD_NONE =
        ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_NONE,
    HRT_LAYER_SHELL_KEYBOARD_EXCLUSIVE =
        ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE,
    HRT_LAYER_SHELL_KEYBOARD_ON_DEMAND =
        ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_ON_DEMAND,
};

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

void hrt_layer_surface_focus(struct hrt_layer_shell_surface *surface,
                             struct hrt_seat *seat);

void hrt_layer_surface_unfocus(struct hrt_layer_shell_surface *surface,
                               struct hrt_seat *seat);

enum hrt_layer_shell_keyboard_interactivity
hrt_layer_surface_keyboard_interactivity(
    struct hrt_layer_shell_surface *surface);

enum zwlr_layer_shell_v1_layer
hrt_layer_surface_layer(struct hrt_layer_shell_surface *surface);

void hrt_layer_surface_position(struct hrt_layer_shell_surface *surface, int *x,
                                int *y);

void hrt_layer_surface_dimensions(struct hrt_layer_shell_surface *surface,
                                  int *width, int *height);

#endif
