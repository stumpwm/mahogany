#ifndef HRT_LAYER_SHELL
#define HRT_LAYER_SHELL

#include <wayland-server-core.h>

struct hrt_layer_shell_surface;

typedef void (*layer_shell_event_handler)(struct hrt_layer_shell_surface *);

struct hrt_layer_shell_callbacks {
    layer_shell_event_handler new_surface;
};

#endif
