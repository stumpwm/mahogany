#include <stdlib.h>
#include <stdio.h>

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>

#include <hrt_input.h>
#include <hrt_server.h>

static void add_new_keyboard(struct hrt_input *input, struct hrt_seat *seat) {
  struct wlr_keyboard *kb = input->wlr_input_device->keyboard;
  wlr_keyboard_set_keymap(kb, seat->keyboard_group->keyboard.keymap);
  wlr_keyboard_group_add_keyboard(seat->keyboard_group, kb);
}

static void add_new_pointer(struct hrt_input *input, struct hrt_seat *seat) {
  wlr_cursor_attach_input_device(seat->cursor, input->wlr_input_device);
}

static void input_device_destroy(struct wl_listener *listener, void *data) {
  puts("input device destroyed");

  struct hrt_input *input = wl_container_of(listener, input, destroy);
  wl_list_remove(&input->link);
  free(input);
}

static void new_input_notify(struct wl_listener *listener, void *data) {
  puts("New input device added");

  struct hrt_seat *seat = wl_container_of(listener,seat, new_input);
  struct wlr_input_device *dev = data;
  struct hrt_input *input = calloc(1, sizeof(struct hrt_input));
  input->wlr_input_device = dev;
  input->seat = seat;
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
  wlr_seat_set_capabilities(seat->seat, caps);
}

bool hrt_seat_init(struct hrt_seat *seat, struct hrt_server *server) {
  seat->server = server;
  seat->new_input.notify = new_input_notify;
  wl_signal_add(&server->backend->events.new_input, &seat->new_input);

  seat->cursor = wlr_cursor_create();
  if(!seat->cursor) {
    return false;
  }
  wlr_cursor_attach_output_layout(seat->cursor, server->output_layout);
  hrt_cursor_init(seat);

  hrt_keyboard_init(seat);

  seat->seat = wlr_seat_create(server->wl_display, "seat-0");
  if(!seat->seat) {
    return false;
  }
  wl_list_init(&seat->inputs);

  return true;
}
