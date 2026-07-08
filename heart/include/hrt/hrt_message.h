#ifndef HRT_HRT_MESSAGE_H
#define HRT_HRT_MESSAGE_H

#include <stdbool.h>

struct hrt_scene_root;
struct hrt_server;
struct hrt_output;

enum window_gravity {
    GRAVITY_TOP_RIGHT = 0,
    GRAVITY_TOP_LEFT,
    GRAVITY_BOTTOM_RIGHT,
    GRAVITY_BOTTOM_LEFT,
    GRAVITY_RIGHT,
    GRAVITY_LEFT,
    GRAVITY_TOP,
    GRAVITY_BOTTOM,
    GRAVITY_CENTER,
    GRAVITY_MAX = GRAVITY_CENTER,
};

struct hrt_message_theme {
    char *font;
    float font_color[4];
    float background_color[4];
    float border_color[4];
    int message_padding;
    int message_border_width;
    int margin_x;
    int margin_y;
};

bool hrt_toast_message(struct hrt_server *server,
                       struct hrt_output *output,
                       const char *text,
                       enum window_gravity gravity,
                       struct hrt_message_theme *theme,
                       int ms_delay);

#endif
