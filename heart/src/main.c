#include <stdio.h>

#include <wlr/util/log.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>

void cursor_callback() {
  puts("Cursor callback called");
}

void output_callback(struct hrt_output *output) {
  puts("Output callback called");
}

bool keyboard_callback() {
  puts("Keyboard callback called");
  return false;
}

static const struct hrt_output_callbacks output_callbacks = {
  .output_added = &output_callback,
  .output_removed = &output_callback,
};

static const struct hrt_seat_callbacks seat_callbacks = {
  .button_event = &cursor_callback,
  .wheel_event = &cursor_callback,
  .keyboard_key_event = &keyboard_callback,
  .keyboard_modifier_event = &keyboard_callback,
};

int main(int argc, char *argv[]) {
  wlr_log_init(WLR_DEBUG, NULL);

  struct hrt_server server;

  if(!hrt_server_init(&server, &output_callbacks, &seat_callbacks)) {
    return 1;
  }

  hrt_server_start(&server);
  hrt_server_run(&server);
  hrt_server_finish(&server);
  return 0;
}
