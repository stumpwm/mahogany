#include <assert.h>
#include <string.h>
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/util/log.h>
#include <stdlib.h>
#include "seat_impl.h"

#include <wlr/backend/libinput.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_primary_selection.h>

#include <hrt/hrt_input.h>
#include <hrt/hrt_server.h>

static void add_new_keyboard(struct hrt_input *input, struct hrt_seat *seat) {
    struct wlr_keyboard *kb =
        wlr_keyboard_from_input_device(input->wlr_input_device);
    wlr_keyboard_set_keymap(kb, seat->keyboard_group->keyboard.keymap);
    if (!wlr_keyboard_group_add_keyboard(seat->keyboard_group, kb)) {
        wlr_log(WLR_ERROR, "Could not add keyboard to keyboard group!");
        exit(1);
    }
}

static void log_config_status(struct libinput_device *ldev, const char *what,
                              enum libinput_config_status status) {
    if (status != LIBINPUT_CONFIG_STATUS_SUCCESS) {
        wlr_log(WLR_ERROR, "Touchpad %s not applied to %s: %s", what,
                libinput_device_get_name(ldev),
                libinput_config_status_to_str(status));
    }
}

static struct libinput_device *get_touchpad_device(struct wlr_input_device *dev) {
    // Null for devices that don't come from libinput, headless, or nested.
    if (!wlr_input_device_is_libinput(dev)) {
        return nullptr;
    }
    struct libinput_device *ldev = wlr_libinput_get_device_handle(dev);
    // libinput only configures tap on touchpads
    // so this is null on a mouse or trackpoint
    if (!ldev || libinput_device_config_tap_get_finger_count(ldev) == 0) {
        return nullptr;
    }
    return ldev;
}

static enum libinput_config_tap_state tap_state(struct hrt_seat *seat,
                                                struct libinput_device *ldev) {
    switch (seat->touchpad.tap) {
        case HRT_TOUCHPAD_ENABLED:
            return LIBINPUT_CONFIG_TAP_ENABLED;
        case HRT_TOUCHPAD_DISABLED:
            return LIBINPUT_CONFIG_TAP_DISABLED;
        case HRT_TOUCHPAD_DEFAULT:
        default:
            return libinput_device_config_tap_get_default_enabled(ldev);
    }
}

static enum libinput_config_dwt_state dwt_state(struct hrt_seat *seat,
                                                struct libinput_device *ldev) {
    switch (seat->touchpad.dwt) {
        case HRT_TOUCHPAD_ENABLED:
            return LIBINPUT_CONFIG_DWT_ENABLED;
        case HRT_TOUCHPAD_DISABLED:
            return LIBINPUT_CONFIG_DWT_DISABLED;
        case HRT_TOUCHPAD_DEFAULT:
        default:
            return libinput_device_config_dwt_get_default_enabled(ldev);
    }
}

static void apply_touchpad_config(struct hrt_input *input) {
    struct hrt_seat *seat        = input->seat;
    struct libinput_device *ldev = get_touchpad_device(input->wlr_input_device);
    if (!ldev) {
        return;
    }
    log_config_status(ldev, "tap-to-click",
                      libinput_device_config_tap_set_enabled(
                          ldev, tap_state(seat, ldev)));
    if (libinput_device_config_dwt_is_available(ldev)) {
        log_config_status(ldev, "disable-while-typing",
                          libinput_device_config_dwt_set_enabled(
                              ldev, dwt_state(seat, ldev)));
    }
    if (libinput_device_config_accel_is_available(ldev)) {
        double speed =
            seat->touchpad.accel_set
                ? seat->touchpad.accel
                : libinput_device_config_accel_get_default_speed(ldev);
        log_config_status(ldev, "acceleration",
                          libinput_device_config_accel_set_speed(ldev, speed));
    }
}

static void reapply_touchpad_config(struct hrt_seat *seat) {
    struct hrt_input *input;
    wl_list_for_each(input, &seat->inputs, link) {
        apply_touchpad_config(input);
    }
}

void hrt_seat_set_touchpad_tap(struct hrt_seat *seat,
                               enum hrt_touchpad_state state) {
    seat->touchpad.tap = state;
    reapply_touchpad_config(seat);
}

void hrt_seat_set_touchpad_dwt(struct hrt_seat *seat,
                               enum hrt_touchpad_state state) {
    seat->touchpad.dwt = state;
    reapply_touchpad_config(seat);
}

void hrt_seat_set_touchpad_accel(struct hrt_seat *seat, double speed) {
    seat->touchpad.accel     = speed;
    seat->touchpad.accel_set = true;
    reapply_touchpad_config(seat);
}

void hrt_seat_reset_touchpad_accel(struct hrt_seat *seat) {
    seat->touchpad.accel     = 0.0;
    seat->touchpad.accel_set = false;
    reapply_touchpad_config(seat);
}

static void add_new_pointer(struct hrt_input *input, struct hrt_seat *seat) {
    wlr_cursor_attach_input_device(seat->cursor, input->wlr_input_device);
    apply_touchpad_config(input);
}

static uint32_t find_input_caps(struct hrt_seat *seat,
                                struct hrt_input *input) {
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

// TODO: do we really need this? Can it be removed?
static void input_device_destroy(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "input device destroyed");

    struct hrt_input *input = wl_container_of(listener, input, destroy);

    /* // Signals */
    wl_list_remove(&input->destroy.link);
    wl_list_remove(&input->link);

    free(input);
}

static void new_input_notify(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "New input device added");

    struct hrt_seat *seat        = wl_container_of(listener, seat, new_input);
    struct wlr_input_device *dev = data;
    struct hrt_input *input      = calloc(1, sizeof(struct hrt_input));
    input->wlr_input_device      = dev;
    input->seat                  = seat;

    /// Signals
    input->destroy.notify = input_device_destroy;
    wl_signal_add(&dev->events.destroy, &input->destroy);

    wl_list_insert(&seat->inputs, &input->link);

    switch (dev->type) {
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

static void handle_request_set_cursor(struct wl_listener *listener,
                                      void *data) {
    struct hrt_seat *seat = wl_container_of(listener, seat, request_cursor);
    struct wlr_seat_pointer_request_set_cursor_event *event = data;

    struct wlr_seat_client *focused = seat->seat->pointer_state.focused_client;
    if (focused == event->seat_client) {
        wlr_cursor_set_surface(seat->cursor, event->surface, event->hotspot_x,
                               event->hotspot_y);
    }
}

static void handle_request_set_selection(struct wl_listener *listener,
                                         void *data) {
    struct hrt_seat *seat = wl_container_of(listener, seat, request_selection);

    struct wlr_seat_request_set_selection_event *event = data;

    wlr_seat_set_selection(seat->seat, event->source, event->serial);
}

static void handle_request_set_primary_selection(struct wl_listener *listener,
                                                 void *data) {
    struct hrt_seat *seat =
        wl_container_of(listener, seat, request_primary_selection);

    struct wlr_seat_request_set_primary_selection_event *event = data;
    wlr_seat_set_primary_selection(seat->seat, event->source, event->serial);
}

static void handle_request_start_drag(struct wl_listener *listener,
                                      void *data) {
    struct hrt_seat *seat = wl_container_of(listener, seat, request_start_drag);
    struct wlr_seat_request_start_drag_event *event = data;

    if (wlr_seat_validate_pointer_grab_serial(seat->seat, event->origin,
                                              event->serial))
        wlr_seat_start_pointer_drag(seat->seat, event->drag, event->serial);
    else
        wlr_data_source_destroy(event->drag->source);
}

static void check_callbacks(const struct hrt_seat_callbacks *callbacks) {
    assert(callbacks->button_event != nullptr);
    assert(callbacks->wheel_event != nullptr);
    assert(callbacks->keyboard_keypress_event != nullptr);
}

static void handle_destroy_drag_icon(struct wl_listener *listener, void *data) {
    struct hrt_drag *hrt_drag = wl_container_of(listener, hrt_drag, destroy);

    if (hrt_drag->icon_tree)
        wlr_scene_node_destroy(&hrt_drag->icon_tree->node);

    wl_list_remove(&hrt_drag->motion.link);
    wl_list_remove(&hrt_drag->destroy.link);

    hrt_seat_reset_view_under(hrt_drag->seat);

    free(hrt_drag);
}

static void handle_drag_motion(struct wl_listener *listener, void *data) {
    struct hrt_drag *drag = wl_container_of(listener, drag, motion);

    wlr_scene_node_set_position(&drag->icon_tree->node, drag->seat->cursor->x,
                                drag->seat->cursor->y);
}

static void handle_start_drag(struct wl_listener *listener, void *data) {
    struct hrt_seat *seat = wl_container_of(listener, seat, start_drag);

    struct wlr_drag *wlr_drag = data;

    struct wlr_drag_icon *wlr_drag_icon = wlr_drag->icon;
    if (!wlr_drag_icon)
        return;

    struct hrt_drag *drag = calloc(1, sizeof(struct hrt_drag));

    if (drag == NULL) {
        wlr_log(WLR_DEBUG, "hrt_drag allocation issue");
        return;
    }

    drag->seat     = seat;
    drag->drag     = wlr_drag;
    wlr_drag->data = drag;

    struct wlr_scene_tree *icon_tree = wlr_scene_drag_icon_create(
        seat->server->scene_root->overlay, wlr_drag_icon);

    if (!icon_tree) {
        wlr_log(WLR_DEBUG, "Failed to allocate drag icon scene tree");
        free(drag);
        return;
    }

    drag->icon_tree = icon_tree;

    drag->motion.notify = handle_drag_motion;
    wl_signal_add(&wlr_drag->events.motion, &drag->motion);
    drag->destroy.notify = handle_destroy_drag_icon;
    wl_signal_add(&wlr_drag->events.destroy, &drag->destroy);

    wlr_scene_node_set_position(&drag->icon_tree->node, drag->seat->cursor->x,
                                drag->seat->cursor->y);
}

static void handle_seat_destroy(struct wl_listener *listener, void *data) {
    // struct wlr_seat *wlr_seat = data;
    struct hrt_seat *seat = wl_container_of(listener, seat, destroy);
    seat->cursor_img_buf_len = 0;
    free(seat->cursor_image);

    hrt_keyboard_destroy(seat);
    hrt_cursor_destroy(seat);
    wlr_log(WLR_DEBUG, "Seat destroyed");

    wl_list_remove(&seat->request_cursor.link);
    wl_list_remove(&seat->request_selection.link);
    wl_list_remove(&seat->request_primary_selection.link);
    wl_list_remove(&seat->request_start_drag.link);
    wl_list_remove(&seat->start_drag.link);
    wl_list_remove(&seat->new_input.link);
    wl_list_remove(&seat->destroy.seat.link);
}

bool hrt_seat_init(struct hrt_seat *seat, struct hrt_server *server,
                   const struct hrt_seat_callbacks *callbacks) {
    check_callbacks(callbacks);
    seat->callbacks        = callbacks;
    seat->server           = server;
    seat->new_input.notify = new_input_notify;
    wl_signal_add(&server->backend->events.new_input, &seat->new_input);

    seat->seat = wlr_seat_create(server->wl_display, "seat-0");
    if (!seat->seat) {
        return false;
    }
    wl_list_init(&seat->inputs);

    seat->request_cursor.notify = handle_request_set_cursor;
    wl_signal_add(&seat->seat->events.request_set_cursor,
                  &seat->request_cursor);

    if (!hrt_cursor_init(seat, server)) {
        return false;
    }

    hrt_keyboard_init(seat);

    seat->request_selection.notify = handle_request_set_selection;
    wl_signal_add(&seat->seat->events.request_set_selection,
                  &seat->request_selection);

    seat->request_primary_selection.notify =
        handle_request_set_primary_selection;
    wl_signal_add(&seat->seat->events.request_set_primary_selection,
                  &seat->request_primary_selection);

    seat->request_start_drag.notify = handle_request_start_drag;
    wl_signal_add(&seat->seat->events.request_start_drag,
                  &seat->request_start_drag);

    seat->start_drag.notify = handle_start_drag;
    wl_signal_add(&seat->seat->events.start_drag, &seat->start_drag);

    seat->destroy.seat.notify = handle_seat_destroy;
    wl_signal_add(&seat->seat->events.destroy, &seat->destroy.seat);

    const char *const default_cursor = "default";
    seat->cursor_img_buf_len = strlen(default_cursor) + 1;
    seat->cursor_image   = calloc(20, sizeof(char));
    memcpy(seat->cursor_image, default_cursor, seat->cursor_img_buf_len);

    return true;
}

void hrt_seat_destroy(struct hrt_seat *seat) {
    wlr_seat_destroy(seat->seat);
}
