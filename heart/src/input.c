#include <stdlib.h>
#include <stdio.h>

#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>

#include <hrt_input.h>
#include <hrt_server.h>

static void new_input_notify(struct wl_listener *listener, void *data) {
  puts("New input device added");
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
