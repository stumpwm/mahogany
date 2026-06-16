#include <hrt/hrt_input.h>

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
    struct wlr_seat *wlr_seat        = seat->seat;
    struct wlr_surface *prev_surface = wlr_seat->keyboard_state.focused_surface;

    if (prev_surface == surface) {
        // Don't re-focus an already focused surface:
        return false;
    }
    struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(wlr_seat);

    if (keyboard != NULL) {
        wlr_seat_keyboard_notify_enter(wlr_seat, surface, keyboard->keycodes,
                                       keyboard->num_keycodes,
                                       &keyboard->modifiers);
        return true;
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
