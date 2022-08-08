#ifndef HRT_HRT_INPUT_H
#define HRT_HRT_INPUT_H

#include <wayland-server.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard_group.h>
#include <xkbcommon/xkbcommon.h>

struct hrt_server;

struct hrt_seat {
  struct hrt_server *server;

  struct wlr_cursor *cursor;
  struct wlr_keyboard_group *keyboard_group;
  struct wlr_xcursor_manager *xcursor_manager;
  struct wlr_seat *seat;
  struct wl_list inputs;
  struct wl_listener new_input;

  // cursor events:
  struct wl_listener motion;
  struct wl_listener motion_absolute;
  struct wl_listener button;
  struct wl_listener axis;
  struct wl_listener frame;

  // keyboard events:
  struct wl_listener keyboard_key;
  struct wl_listener keyboard_modifiers;

  const struct hrt_seat_callbacks *callbacks;
  char *cursor_image;
};

struct hrt_keypress_info {
  const xkb_keysym_t *keysyms;
  uint32_t modifiers;
  size_t keysyms_len;
};

struct hrt_seat_callbacks {
  // TODO: these need parameters
  void (*button_event)();
  void (*wheel_event)();
  // We will eventually want to pass in the event object, keyboard object and seat object
  // to get anything beyond basic keybindings, but then this should work for the basics
  // and we don't need to write wlroots bindings.
  /**
   * This callback is called whenever a non-modifier key is pressed (not released)
   **/
  bool (*keyboard_keypress_event)(struct hrt_keypress_info *info);
};

struct hrt_input {
  struct wlr_input_device *wlr_input_device;
  struct hrt_seat *seat;
  struct wl_list link;

  struct wl_listener destroy;
};

bool hrt_seat_init(struct hrt_seat *seat, struct hrt_server *server,
		   const struct hrt_seat_callbacks *callbacks);
bool hrt_cursor_init(struct hrt_seat *seat, struct hrt_server *server);
void hrt_keyboard_init(struct hrt_seat *seat);

/**
 * Set the seat's default cursor image to the given cursor name.
 *
 * Does not take ownership of the string.
 *
 * See themes section of man xcursor(3) to find where to find valid cursor names.
 */
void hrt_seat_set_cursor_img(struct hrt_seat *seat, char *img_name);

#endif
