#include <hrt/hrt_input.h>

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_seat.h>

void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name) {
    seat->cursor_image = img_name;
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

double hrt_seat_cursor_lx(struct hrt_seat *seat) {
  return seat->cursor->x;
}

double hrt_seat_cursor_ly(struct hrt_seat *seat) {
  return seat->cursor->y;
}
