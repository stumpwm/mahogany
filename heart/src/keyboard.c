#include <stdio.h>
#include <stdlib.h>

// Temp: needed for exiting on escape key pressed:
#include <xkbcommon/xkbcommon-keysyms.h>
#include <xkbcommon/xkbcommon-names.h>
#include <wlr/types/wlr_seat.h>

#include <wlr/backend/multi.h>
#include <wlr/backend/session.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_input.h>

static size_t seat_translate_keysyms(struct hrt_seat *seat, xkb_keycode_t keycode,
			       const xkb_keysym_t **keysyms, uint32_t *modifiers) {
  struct wlr_input_device *device = seat->keyboard_group->input_device;
  *modifiers = wlr_keyboard_get_modifiers(device->keyboard);
  xkb_mod_mask_t consumed = xkb_state_key_get_consumed_mods2(device->keyboard->xkb_state,
							     keycode,
							     XKB_CONSUMED_MODE_XKB);
  *modifiers = *modifiers & ~consumed;

  return xkb_state_key_get_syms(device->keyboard->xkb_state, keycode, keysyms);
}

static bool execute_hardcoded_bindings(struct hrt_server *server,
				       const xkb_keysym_t *pressed_keysyms, uint32_t modifiers,
				       size_t keysyms_len) {
  for(size_t i = 0; i < keysyms_len; ++i) {
    xkb_keysym_t keysym = pressed_keysyms[i];
    if (keysym == XKB_KEY_Escape) {
      exit(0);
    }

    if (keysym >= XKB_KEY_XF86Switch_VT_1 && keysym <= XKB_KEY_XF86Switch_VT_12) {
      if (wlr_backend_is_multi(server->backend)) {
	struct wlr_session *session = wlr_backend_get_session(server->backend);
	if (session) {
	  puts("Changing session");
	  unsigned vt = keysym - XKB_KEY_XF86Switch_VT_1 + 1;
	  wlr_session_change_vt(session, vt);
	}
      }
      return true;
    }
  }
  return false;
}

static void seat_handle_key(struct wl_listener *listener, void *data) {
  puts("Keyboard key pressed");
  struct hrt_seat *seat = wl_container_of(listener, seat, keyboard_key);
  struct wlr_event_keyboard_key *event = data;
  struct hrt_server *server = seat->server;

  xkb_keycode_t keycode = event->keycode + 8;

  const xkb_keysym_t *translated_keysyms;
  uint32_t translated_modifiers;
  size_t translated_keysyms_len = seat_translate_keysyms(seat, keycode,
							 &translated_keysyms, &translated_modifiers);

  bool handled = false;

  if(event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
    struct hrt_keypress_info key_info = {
      .keysyms = translated_keysyms,
      .keysyms_len = translated_keysyms_len,
      .modifiers = translated_modifiers
    };
    handled = seat->callbacks->keyboard_keypress_event(&key_info);
  }

  if(!handled && event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
    handled = execute_hardcoded_bindings(server, translated_keysyms, translated_modifiers,
					 translated_keysyms_len);
  }

  // TODO: I don't know if this condition is correct
  if(!handled || event->state == WL_KEYBOARD_KEY_STATE_RELEASED) {
    wlr_seat_keyboard_notify_key(seat->seat, event->time_msec, event->keycode, event->state);
  }
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
