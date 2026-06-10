#ifndef RENDER_SCAL_UTIL_H
#define RENDER_SCAL_UTIL_H

#include <wlr/util/box.h>
#include <wlr/types/wlr_buffer.h>

/**
 * Use the given width and height to populate given box at the given scale
 **/
bool compute_scaled_box(int width, int height, double scale, struct wlr_box *box);

/**
 * Compute the appropriate scale filter to use for the given buffer and
 * scaled box.
 **/
enum wlr_scale_filter_mode compute_scale_filter(struct wlr_buffer *base,
                                                struct wlr_box *message_box,
                                                double scale);

#endif
