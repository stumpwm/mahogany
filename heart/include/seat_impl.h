#pragma once

#include <stdbool.h>
#include <wlr/types/wlr_scene.h>

#include "hrt/hrt_server.h"

struct hrt_drag {
    struct hrt_seat *seat;
    struct wlr_drag *drag;
    struct wlr_scene_tree *icon_tree;

    struct wl_listener motion;
    struct wl_listener destroy;
};

bool hrt_seat_init(struct hrt_seat *seat, struct hrt_server *server,
		   const struct hrt_seat_callbacks *callbacks);
void hrt_seat_destroy(struct hrt_seat *seat);

void hrt_keyboard_init(struct hrt_seat *seat);
void hrt_keyboard_destroy(struct hrt_seat *seat);

bool hrt_cursor_init(struct hrt_seat *seat, struct hrt_server *server);
void hrt_cursor_destroy(struct hrt_seat *seat);
