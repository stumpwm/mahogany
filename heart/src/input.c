#include <wlr/util/log.h>
#include <stdlib.h>
#include <stdio.h>

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>

#include <hrt/hrt_input.h>
#include <hrt/hrt_server.h>

static void add_new_keyboard(struct hrt_input *input, struct hrt_seat *seat) {
  struct wlr_keyboard *kb = wlr_keyboard_from_input_device(input->wlr_input_device);
  wlr_keyboard_set_keymap(kb, seat->keyboard_group->keyboard.keymap);
  if(!wlr_keyboard_group_add_keyboard(seat->keyboard_group, kb)) {
    wlr_log(WLR_ERROR, "Could not add keyboard to keyboard group!");
    exit(1);
  }
}

static void remove_keyboard(struct hrt_input *input, struct hrt_seat *seat) {
	struct wlr_keyboard *kb = wlr_keyboard_from_input_device(input->wlr_input_device);
  wlr_keyboard_group_remove_keyboard(seat->keyboard_group, kb);
}

static void add_new_pointer(struct hrt_input *input, struct hrt_seat *seat) {
  wlr_cursor_attach_input_device(seat->cursor, input->wlr_input_device);
}

static void remove_pointer(struct hrt_input *input, struct hrt_seat *seat) {
  wlr_cursor_detach_input_device(seat->cursor, input->wlr_input_device);
}

static uint32_t find_input_caps(struct hrt_seat *seat, struct hrt_input *input) {
  uint32_t caps = 0;
  wl_list_for_each(input, &seat->inputs, link) {
    switch (input->wlr_input_device->type) {
    case WLR_INPUT_DEVICE_KEYBOARD:
      caps |= WL_SEAT_CAPABILITY_KEYBOARD;
      break;
    case WLR_INPUT_DEVICE_POINTER:
      caps |= WL_SEAT_CAPABILITY_POINTER;
      break;
    case WLR_INPUT_DEVICE_TOUCH:
      caps |= WL_SEAT_CAPABILITY_TOUCH;
      break;
    default:
      /* This space deliberately left blank */
      break;
    }
  }
  return caps;
}

static void input_device_destroy(struct wl_listener *listener, void *data) {
  wlr_log(WLR_DEBUG, "input device destroyed");

  struct hrt_input *input = wl_container_of(listener, input, destroy);

  switch(input->wlr_input_device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    remove_keyboard(input, input->seat);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    remove_pointer(input, input->seat);
    break;
  default:
    break;
  }

  // Signals
  wl_list_remove(&input->destroy.link);

  wl_list_remove(&input->link);


  uint32_t caps = find_input_caps(input->seat, input);
  wlr_seat_set_capabilities(input->seat->seat, caps);

  free(input);
}

static void new_input_notify(struct wl_listener *listener, void *data) {
  wlr_log(WLR_DEBUG, "New input device added");

  struct hrt_seat *seat = wl_container_of(listener,seat, new_input);
  struct wlr_input_device *dev = data;
  struct hrt_input *input = calloc(1, sizeof(struct hrt_input));
  input->wlr_input_device = dev;
  input->seat = seat;

  /// Signals
  input->destroy.notify = input_device_destroy;
  wl_signal_add(&dev->events.destroy, &input->destroy);

  wl_list_insert(&seat->inputs, &input->link);

  switch(dev->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    add_new_keyboard(input, seat);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    add_new_pointer(input, seat);
    break;
  default:
    break;
  }

  uint32_t caps = find_input_caps(seat, input);
  wlr_seat_set_capabilities(seat->seat, caps);
}

bool hrt_seat_init(struct hrt_seat *seat, struct hrt_server *server,
		   const struct hrt_seat_callbacks *callbacks) {
  seat->callbacks = callbacks;
  seat->server = server;
  seat->new_input.notify = new_input_notify;
  wl_signal_add(&server->backend->events.new_input, &seat->new_input);

  seat->seat = wlr_seat_create(server->wl_display, "seat-0");
  if(!seat->seat) {
    return false;
  }
  wl_list_init(&seat->inputs);

  if(!hrt_cursor_init(seat, server)) {
     return false;
  }

  hrt_keyboard_init(seat);

  seat->cursor_image = "left_ptr";

  return true;
}

void hrt_seat_destroy(struct hrt_seat *seat) {
  wlr_seat_destroy(seat->seat);
  hrt_keyboard_destroy(seat);
  hrt_cursor_destroy(seat);
}
