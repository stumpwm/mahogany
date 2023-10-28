#include <stdio.h>

#include <string.h>
#include <wlr/util/log.h>
#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>
#include <hrt/hrt_view.h>

static void cursor_callback(struct hrt_seat *seat) {
  puts("Cursor callback called");
}

static void output_callback(struct hrt_output *output) {
  puts("Output callback called");
}

static void new_view_callback(struct hrt_view *view) {
  puts("New view callback called!");
}

static void view_destroy_callback(struct hrt_view *view) {
  puts("View destroy callback called");
}

static bool showNormalCursor = true;
static bool keyboard_callback(struct hrt_seat *seat, struct hrt_keypress_info *info) {
  puts("Keyboard callback called");
  printf("Modifiers: %d\n", info->modifiers);
  printf("Keys pressed:");
  for(size_t i = 0; i < info->keysyms_len; ++i) {
	  if (info->keysyms[i] == XKB_KEY_Escape) {
		  puts("Exiting due to escape pressed");
		  hrt_server_stop(seat->server);
	  }
	  char buffer[20];
	  xkb_keysym_get_name(info->keysyms[i], buffer, sizeof(buffer));
	  printf(" %s", buffer);
    if(strcmp(buffer, "c") == 0) {
      hrt_seat_set_cursor_img(seat, showNormalCursor ? "crossed_circle" : "left_ptr");
      showNormalCursor = !showNormalCursor;
    }
  }
  puts("\n\n");
  return false;
}

static const struct hrt_output_callbacks output_callbacks = {
  .output_added = &output_callback,
  .output_removed = &output_callback,
};

static const struct hrt_seat_callbacks seat_callbacks = {
    .button_event = &cursor_callback,
    .wheel_event = &cursor_callback,
    .keyboard_keypress_event = &keyboard_callback,
};

static const struct hrt_view_callbacks view_callbacks = {
	.new_view = &new_view_callback,
	.view_destroyed = &view_destroy_callback,
};

int main(int argc, char *argv[]) {
  wlr_log_init(WLR_DEBUG, NULL);

  struct hrt_server server;

  if(!hrt_server_init(&server, &output_callbacks, &seat_callbacks, &view_callbacks, WLR_DEBUG)) {
    return 1;
  }

  hrt_server_start(&server);
  hrt_server_finish(&server);
  return 0;
}
