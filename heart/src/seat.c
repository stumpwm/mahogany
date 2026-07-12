#include <stdlib.h>
#include <string.h>

#include <hrt/hrt_input.h>
#include <seat_impl.h>

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>

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

bool hrt_seat_keyboard_focus_surface(struct hrt_seat *seat,
                                     struct wlr_surface *surface) {
    struct wlr_seat *wlr_seat = seat->seat;
    if (!seat->grabbed) {
        struct wlr_surface *prev_surface =
            wlr_seat->keyboard_state.focused_surface;
        if (prev_surface == surface) {
            // Don't re-focus an already focused surface:
            return false;
        }
        struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(wlr_seat);

        if (keyboard != NULL) {
            wlr_seat_keyboard_notify_enter(
                wlr_seat, surface, keyboard->keycodes, keyboard->num_keycodes,
                &keyboard->modifiers);
            return true;
        }
    }
    return false;
}

bool hrt_seat_keyboard_focus_surface_clear(struct hrt_seat *seat,
                                           struct wlr_surface *surface) {
    struct wlr_seat *wlr_seat        = seat->seat;
    struct wlr_surface *prev_surface = wlr_seat->keyboard_state.focused_surface;

    if (prev_surface != surface) {
        // Don't clear focus if this isn't the focused surface:
        return false;
    }
    wlr_seat_keyboard_notify_clear_focus(seat->seat);
    return true;
};

double hrt_seat_cursor_lx(struct hrt_seat *seat) {
    return seat->cursor->x;
}

double hrt_seat_cursor_ly(struct hrt_seat *seat) {
    return seat->cursor->y;
}

static void seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
    // Ensure that our buffer is big enough,
    // unless the string is super big
    const size_t len = strlen(img_name) + 1;
    if (len > seat->cursor_img_buf_len) {
        if (len > 100) {
            wlr_log(WLR_ERROR,
                    "Ignoring hrt_set_cursor_img: cursor name is unreasonable "
                    "large: %s",
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
}

/**
 * Set the seat's default cursor image to the given cursor name.
 *
 * Does not take ownership of the string.
 *
 * See themes section of man xcursor(3) to find where to find valid cursor
 * names.
 */
static void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
    seat_set_cursor_img(seat, img_name);
    wlr_cursor_set_xcursor(seat->cursor, seat->xcursor_manager,
                           seat->cursor_image);
}

/**
 * Set the cursor image back to the default.
 **/
static void hrt_seat_reset_cursor_img(struct hrt_seat *seat) {
    seat_set_cursor_img(seat, "default");
    // TODO: investigate; this may send the pointer_enter event
    // when the pointer is already in the view. Does that matter?
    hrt_seat_reset_view_under(seat);
}

void hrt_seat_grab(struct hrt_seat *seat, char *img_name) {
    hrt_seat_set_cursor_img(seat, img_name);
    wlr_seat_keyboard_notify_clear_focus(seat->seat);
    wlr_seat_pointer_notify_clear_focus(seat->seat);
    hrt_seat_disable_cursor_events(seat);
    seat->grabbed = true;
}

void hrt_seat_ungrab(struct hrt_seat *seat) {
    seat->grabbed = false;
    hrt_seat_enable_cursor_events(seat);
    hrt_seat_reset_cursor_img(seat);
    hrt_seat_reset_view_under(seat);
}
