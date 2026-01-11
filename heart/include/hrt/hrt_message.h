#ifndef HRT_HRT_MESSAGE_H
#define HRT_HRT_MESSAGE_H

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

bool hrt_toast_message(struct hrt_server *server,
                       struct hrt_scene_root *scene_root,
                       struct hrt_output *output,
                       const char *text,
                       enum window_gravity gravity,
                       int margin_x,
                       int margin_y,
                       int ms_delay);

#endif
