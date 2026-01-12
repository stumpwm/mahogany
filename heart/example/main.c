#include "hrt/hrt_scene.h"
#include <stdio.h>

#include <string.h>
#include <wlr/util/log.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>
#include <hrt/hrt_view.h>
#include <hrt/hrt_message.h>

struct example_server {
    struct hrt_server server;
    struct hrt_scene_root *scene_root;
    struct hrt_scene_group *group;
    struct hrt_output *current_output;
    int message_counter;
    enum window_gravity message_gravity;
};

static struct example_server server = {0};

static void cursor_button_callback(struct hrt_seat *seat,
                                   struct wlr_pointer_button_event *event) {
    puts("Cursor callback called");
}

static void cursor_wheel_callback(struct hrt_seat *seat,
                                  struct wlr_pointer_axis_event *event) {
    puts("Cursor callback called");
}

static void output_added_callback(struct hrt_output *output) {
    printf("Output added callback called, scale=%f\n", output->wlr_output->scale);
    server.current_output = output;
}

static void output_removed_callback(struct hrt_output *output) {
    puts("Output removed callback called");
    if (server.current_output == output)
        server.current_output = NULL;
}

static void new_view_callback(struct hrt_view *view) {
    puts("New view callback called!");
}

static void view_destroy_callback(struct hrt_view *view) {
    puts("View destroy callback called");
}

static bool showNormalCursor = true;

static const char *gravity_names[] = {
    "top right",
    "top left",
    "bottom right",
    "bottom left",
    "right",
    "left",
    "top",
    "bottom",
    "center"
};

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
        if (strcmp(buffer, "c") == 0) {
            hrt_seat_set_cursor_img(
                seat, showNormalCursor ? "crossed_circle" : "left_ptr");
            showNormalCursor = !showNormalCursor;
        } else if(strcmp(buffer, "m") == 0 && server.current_output) {
            char text[64];
            snprintf(text, sizeof(text) - 1, "test message %u\ngravity: %s",
                     server.message_counter, gravity_names[server.message_gravity]);
            if (hrt_toast_message(
                 &server.server, server.scene_root, server.current_output,
                 text, server.message_gravity, 15, 15, 5000)) {
                server.message_counter++;
                server.message_gravity++;
                if (server.message_gravity > GRAVITY_MAX)
                    server.message_gravity = 0;
            }
        }
    }
    puts("\n\n");
    return false;
}

static void layout_changed() {}

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
    .new_view       = &new_view_callback,
    .view_destroyed = &view_destroy_callback,
};

int main(int argc, char *argv[]) {
    wlr_log_init(WLR_DEBUG, NULL);

    if (!hrt_server_init(&server.server, &output_callbacks, &seat_callbacks,
                         &view_callbacks, WLR_DEBUG)) {
        return 1;
    }

    server.scene_root = hrt_scene_root_create(&server.server.scene->tree);
    server.group = hrt_scene_group_create(server.scene_root);

    hrt_server_start(&server.server);
    hrt_server_finish(&server.server);
    return 0;
}
