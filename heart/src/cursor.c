#include "hrt/hrt_view.h"
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_seat.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_input.h>

// This function is shamelessly ripped from the tinywl implementation:
static struct hrt_view *find_view_at(struct hrt_server *server, double lx,
                                         double ly,
                                         struct wlr_surface **surface,
                                         double *sx, double *sy) {
	/* This returns the topmost node in the scene at the given layout coords.
	 * We only care about surface nodes as we are specifically looking for a
	 * surface in the surface tree of a tinywl_toplevel. */
	struct wlr_scene_node *node = wlr_scene_node_at(
		&server->scene->tree.node, lx, ly, sx, sy);
	if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
		return NULL;
	}
	struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
	struct wlr_scene_surface *scene_surface =
		wlr_scene_surface_try_from_buffer(scene_buffer);
	if (!scene_surface) {
		return NULL;
	}

	*surface = scene_surface->surface;
	/* Find the node corresponding to the toplevel at the root of this
	 * surface tree, it is the only one for which we set the data field. */
	struct wlr_scene_tree *tree = node->parent;
	while (tree != NULL && tree->node.data == NULL) {
		tree = tree->node.parent;
	}
	return tree->node.data;
}

static void handle_cursor_motion(struct hrt_seat *seat, uint32_t time) {
	double sx, sy;
	struct wlr_surface *found_surface = NULL;
	struct hrt_view *view = find_view_at(
		seat->server, seat->cursor->x, seat->cursor->y, &found_surface, &sx, &sy);
	if (!view) {
		wlr_cursor_set_xcursor(seat->cursor, seat->xcursor_manager,
													 seat->cursor_image);
  }
	if(found_surface) {
		wlr_seat_pointer_notify_enter(seat->seat, found_surface, sx, sy);
		wlr_seat_pointer_notify_motion(seat->seat, time, sx, sy);
	} else {
		wlr_seat_pointer_clear_focus(seat->seat);
	}
}

static void seat_motion(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, motion);
  struct wlr_pointer_motion_event *ev = data;

  wlr_cursor_move(seat->cursor, &ev->pointer->base, ev->delta_x, ev->delta_y);
  handle_cursor_motion(seat, ev->time_msec);
}

static void seat_motion_absolute(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, motion_absolute);
  struct wlr_pointer_motion_absolute_event *ev = data;

  wlr_cursor_warp_absolute(seat->cursor, &ev->pointer->base, ev->x, ev->y);
  handle_cursor_motion(seat, ev->time_msec);
}

static void seat_button(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, button);
	struct wlr_pointer_button_event *event = data;
	/* Notify the client with pointer focus that a button press has occurred */
	wlr_seat_pointer_notify_button(seat->seat,
	 event->time_msec, event->button, event->state);
  seat->callbacks->button_event(seat);
}

static void seat_axis(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, axis);
  struct wlr_pointer_axis_event *ev = data;

	wlr_seat_pointer_notify_axis(seat->seat,
		ev->time_msec, ev->orientation, ev->delta,
		ev->delta_discrete, ev->source, ev->relative_direction);

  seat->callbacks->wheel_event(seat);
}

static void seat_frame(struct wl_listener *listener, void *data) {
  struct hrt_seat *seat = wl_container_of(listener, seat, frame);
  wlr_seat_pointer_notify_frame(seat->seat);
}

bool hrt_cursor_init(struct hrt_seat *seat, struct hrt_server *server) {
  seat->cursor = wlr_cursor_create();
  if(!seat->cursor) {
    return false;
  }
  wlr_cursor_attach_output_layout(seat->cursor, server->output_layout);

  seat->xcursor_manager = wlr_xcursor_manager_create(NULL, 24);
  if(!seat->xcursor_manager) {
    return false;
  }
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

  return true;
}

void hrt_cursor_destroy(struct hrt_seat *seat) {
	wlr_xcursor_manager_destroy(seat->xcursor_manager);
	wlr_cursor_destroy(seat->cursor);
}
