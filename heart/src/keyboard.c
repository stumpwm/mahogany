#include <stdio.h>

#include <wlr/types/wlr_seat.h>

#include <hrt_input.h>

static void seat_handle_key(struct wl_listener *listener, void *data) {
  puts("Keyobard key pressed");
  struct hrt_seat *seat = wl_container_of(listener, seat, keyboard_key);
  struct wlr_event_keyboard_key *ev = data;

  wlr_seat_keyboard_notify_key(seat->seat, ev->time_msec, ev->keycode, ev->state);
}

static void seat_handle_modifiers(struct wl_listener *listener, void *data) {
  puts("Keyboard modifier pressed");
  struct hrt_seat *seat = wl_container_of(listener, seat, keyboard_modifiers);

  wlr_seat_keyboard_notify_modifiers(seat->seat, &seat->keyboard_group->keyboard.modifiers);
}

void hrt_keyboard_init(struct hrt_seat *seat) {
  seat->keyboard_group = wlr_keyboard_group_create();
  struct wlr_keyboard *kb = &seat->keyboard_group->keyboard;

  struct xkb_rule_names rules = { 0 };
  struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap = xkb_map_new_from_names(context, &rules,
						     XKB_KEYMAP_COMPILE_NO_FLAGS);
  wlr_keyboard_set_keymap(kb, keymap);
  xkb_keymap_unref(keymap);
  xkb_context_unref(context);

  seat->keyboard_key.notify = seat_handle_key;
  wl_signal_add(&kb->events.key, &seat->keyboard_key);
  seat->keyboard_modifiers.notify = seat_handle_modifiers;
  wl_signal_add(&kb->events.modifiers, &seat->keyboard_modifiers);
}
