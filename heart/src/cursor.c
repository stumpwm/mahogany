#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>

#include <hrt/hrt_input.h>

static void seat_motion(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, motion);
  struct wlr_event_pointer_motion *ev = data;
  wlr_xcursor_manager_set_cursor_image(seat->xcursor_manager,
				       "left_ptr", seat->cursor);

  wlr_cursor_move(seat->cursor, ev->device, ev->delta_x, ev->delta_y);
}

static void seat_motion_absolute(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, motion_absolute);
  struct wlr_event_pointer_motion_absolute *ev = data;
  wlr_xcursor_manager_set_cursor_image(seat->xcursor_manager,
				       "left_ptr", seat->cursor);
  wlr_cursor_warp_absolute(seat->cursor, ev->device, ev->x, ev->y);
}

static void seat_button(struct wl_listener *listener, void *data) {

}

static void seat_axis(struct wl_listener *listener, void *data) {

}

static void seat_frame(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, frame);
  wlr_seat_pointer_notify_frame(seat->seat);
}

void hrt_cursor_init(struct hrt_seat *seat) {
  seat->xcursor_manager = wlr_xcursor_manager_create(NULL, 24);
  wlr_xcursor_manager_load(seat->xcursor_manager, 1);

  seat->motion.notify = seat_motion;
  wl_signal_add(&seat->cursor->events.motion, &seat->motion);
  seat->motion_absolute.notify = seat_motion_absolute;
  wl_signal_add(&seat->cursor->events.motion_absolute, &seat->motion_absolute);
  seat->button.notify = seat_button;
  wl_signal_add(&seat->cursor->events.button, &seat->button);
  seat->axis.notify = seat_axis;
  wl_signal_add(&seat->cursor->events.axis, &seat->axis);
  seat->frame.notify = seat_frame;
  wl_signal_add(&seat->cursor->events.frame, &seat->frame);
}
