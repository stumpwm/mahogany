#include "hrt/hrt_border_box.h"
#include "hrt/hrt_scene.h"
#include "hrt/hrt_theme.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wayland-util.h>
#include <wlr/util/log.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>
#include <hrt/hrt_view.h>
#include <hrt/hrt_message.h>

struct example_output {
    struct hrt_output *output;
    struct hrt_border_box *box;
    struct wl_list link;
};

struct example_server {
    struct hrt_server server;
    struct hrt_scene_group *group;
    struct wlr_scene_tree *layer;
    struct wl_list outputs;
    struct example_output *current_output;
    int message_counter;
    enum window_gravity message_gravity;
};

static struct example_server server = {0};

static struct wl_list *next_output() {
    struct wl_list *next = server.current_output->link.next;
    if (next == &server.outputs)
        next = server.outputs.next;
    return next;
}

static void cursor_button_callback(struct hrt_seat *seat,
                                   struct wlr_pointer_button_event *event) {
    puts("Cursor callback called");
}

static void cursor_wheel_callback(struct hrt_seat *seat,
                                  struct wlr_pointer_axis_event *event) {
    puts("Cursor callback called");
}

static struct hrt_border_box_style *first_style  = NULL;
static struct hrt_border_box_style *second_style = NULL;

static void output_added_callback(struct hrt_output *output) {
    printf("Output added callback called, scale=%f\n",
           output->wlr_output->scale);
    struct hrt_output_config config = {
        .scale       = 2,
        .custom_mode = true,
        .width       = 600,
        .height      = 600,
        .refresh_rate = 0,
        .custom_position = false,
    };
    hrt_output_init(output, &config);

    struct example_output *o = calloc(1, sizeof(*o));
    o->output                = output;

    int width, height;
    hrt_output_resolution(output, &width, &height);
    int x, y;
    hrt_output_position(output, &x, &y);
    o->box = hrt_border_box_create(server.layer, first_style, x, y,
                                   width, height);

    wl_list_insert(&server.outputs, &o->link);
    server.current_output = o;
}

static void output_removed_callback(struct hrt_output *output) {
    puts("Output removed callback called");
    struct example_output *o = NULL, *tmp;

    wl_list_for_each_safe(o, tmp, &server.outputs, link) {
        if (o->output == output) {
            if (o == server.current_output) {
                server.current_output = (wl_list_length(&server.outputs) > 1) ?
                    wl_container_of(next_output(), server.current_output,
                                    link) :
                    NULL;
            }
            hrt_border_box_destroy(o->box);
            wl_list_remove(&o->link);
            free(o);
            break;
        }
    }
}

static void new_view_callback(struct hrt_view *view) {
    puts("New view callback called!");
}

static void view_mapped(struct hrt_view *view) {
    puts("View Mapped");
}

static void view_unmapped(struct hrt_view *view) {
    puts("View unmapped");
}

static void view_size_changed(struct hrt_view *view) {
    puts("View size changed");
}

static void view_callback(struct hrt_view *view) {
    puts("Generic callback called");
}

static bool view_fullscreen_callback(struct hrt_view *view,
                                     struct hrt_output *output, bool set) {
    puts("Generic callback called");
    return false;
}

static void view_destroy_callback(struct hrt_view *view) {
    puts("View destroy callback called");
}

static bool border_flag = false;

static void change_border_color() {
    struct hrt_border_box_style *style =
        border_flag ? first_style : second_style;
    /* struct example_output *o = NULL, *tmp; */
    /* wl_list_for_each_safe(o, tmp, &server.outputs, link) { */
    /*     hrt_border_box_set_style(o->box, style); */
    /* } */
    hrt_border_box_set_style(server.current_output->box, style);
    border_flag = !border_flag;
}

static bool keyboard_callback(struct hrt_seat *seat,
                              struct hrt_keypress_info *info) {
    puts("Keyboard callback called");
    printf("Modifiers: %d\n", info->modifiers);
    printf("Keys pressed:");
    for (size_t i = 0; i < info->keysyms_len; ++i) {
        if (info->keysyms[i] == XKB_KEY_Escape) {
            puts("Exiting due to escape pressed");
            hrt_server_stop(seat->server);
        }
        char buffer[20];
        xkb_keysym_get_name(info->keysyms[i], buffer, sizeof(buffer));
        printf(" %s", buffer);
        if (strcmp(buffer, "b") == 0 && server.current_output != NULL) {
            change_border_color();
        } else if (strcmp(buffer, "o") == 0 &&
                   wl_list_length(&server.outputs) > 1) {
            server.current_output =
                wl_container_of(next_output(), server.current_output, link);
            printf("selected output %s\n",
                   server.current_output->output->wlr_output->name);
        }
    }
    puts("\n\n");
    return false;
}

static void layout_changed() {
    struct example_output *o = NULL, *tmp;
    wl_list_for_each_safe(o, tmp, &server.outputs, link) {
        int width, height;
        hrt_output_resolution(o->output, &width, &height);
        hrt_border_box_set_size(o->box, width, height);
        int x, y;
        hrt_output_position(o->output, &x, &y);
        hrt_border_box_set_relative(o->box, x, y);
    }
}

static const struct hrt_output_callbacks output_callbacks = {
    .output_added          = &output_added_callback,
    .output_removed        = &output_removed_callback,
    .output_layout_changed = &layout_changed,
};

static const struct hrt_seat_callbacks seat_callbacks = {
    .button_event            = &cursor_button_callback,
    .wheel_event             = &cursor_wheel_callback,
    .keyboard_keypress_event = &keyboard_callback,
};

static const struct hrt_view_callbacks view_callbacks = {
    .new_view           = &new_view_callback,
    .view_destroyed     = &view_destroy_callback,
    .view_size_changed  = &view_size_changed,
    .view_mapped        = &view_mapped,
    .view_unmapped      = &view_unmapped,
    .request_minimize   = &view_callback,
    .request_maximize   = &view_callback,
    .request_fullscreen = &view_fullscreen_callback,
};

int main(int argc, char *argv[]) {
    wlr_log_init(WLR_DEBUG, NULL);

    wl_list_init(&server.outputs);

    if (!hrt_server_init(&server.server, &output_callbacks, &seat_callbacks,
                         &view_callbacks, NULL, WLR_DEBUG)) {
        return 1;
    }

    server.group = hrt_server_group_create(&server.server);
    server.layer = hrt_scene_layer_create(server.group);

    float first_colors[4]  = {1.0, 1.0, 1.0, 1.0};
    float second_colors[4] = {1, 0, 1, 1};

    first_style =
        hrt_border_box_style_create(HRT_BORDER_DOTTED, first_colors, 1.5);
    second_style =
        hrt_border_box_style_create(HRT_BORDER_SOLID, second_colors, 1);

    hrt_server_start(&server.server);
    hrt_server_finish(&server.server);

    hrt_border_box_style_unref(first_style);
    hrt_border_box_style_unref(second_style);
    return 0;
}
