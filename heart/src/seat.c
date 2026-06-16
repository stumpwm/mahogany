#include <hrt/hrt_input.h>

#include <stdlib.h>
#include <string.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
    // Ensure that our buffer is big enough,
    // unless the string is super big
    const size_t len = strlen(img_name) + 1;
    if (len > seat->cursor_img_buf_len) {
        if (len > 100) {
            wlr_log(WLR_ERROR, "Ignoring hrt_set_cursor_img: cursor name is unreasonable large: %s",
                   img_name);
          return;
        }
        char *new_ptr = realloc(seat->cursor_image, len);
        if (!new_ptr) {
            wlr_log(WLR_ERROR, "Failed to realloc cursor name buffer");
            return;
        }
    }
    memcpy(seat->cursor_image, img_name, len);

    wlr_cursor_set_xcursor(seat->cursor, seat->xcursor_manager,
                           seat->cursor_image);
}

void hrt_seat_notify_button(struct hrt_seat *seat,
                            struct wlr_pointer_button_event *event) {
    wlr_seat_pointer_notify_button(seat->seat, event->time_msec, event->button,
                                   event->state);
}

void hrt_seat_notify_axis(struct hrt_seat *seat,
                          struct wlr_pointer_axis_event *event) {
    wlr_seat_pointer_notify_axis(
        seat->seat, event->time_msec, event->orientation, event->delta,
        event->delta_discrete, event->source, event->relative_direction);
}

bool hrt_seat_set_keymap(struct hrt_seat *seat, struct xkb_rule_names *rules,
                         enum xkb_keymap_compile_flags flags) {
    struct xkb_keymap *keymap =
        xkb_keymap_new_from_names(seat->xkb_context, rules, flags);
    if (keymap) {
        wlr_keyboard_set_keymap(&seat->keyboard_group->keyboard, keymap);
        xkb_keymap_unref(keymap);
        return true;
    } else {
        return false;
    }
}

double hrt_seat_cursor_lx(struct hrt_seat *seat) {
    return seat->cursor->x;
}

double hrt_seat_cursor_ly(struct hrt_seat *seat) {
    return seat->cursor->y;
}
