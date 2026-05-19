#include "hrt/hrt_border_box.h"
#include "hrt/hrt_theme.h"
#include "render/cairo_buffer.h"
#include "render/scale_util.h"
#include "scene_descriptor.h"
#include "wlr/util/log.h"

#include <assert.h>
#include <stdatomic.h>
#include <stdlib.h>
#include <drm_fourcc.h>
#include <cairo.h>

#include <string.h>
#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/interfaces/wlr_buffer.h>

struct hrt_border_box_style {
    enum hrt_border_style border_style;
    float stroke_color[4];
    float line_width;
    atomic_uint refcount;
    struct wl_list boxes;
};

struct hrt_border_box {
    struct wlr_scene_buffer *scene_buffer;
    struct cairo_buffer *buffer;
    struct hrt_border_box_style *style;
    float scale;

    struct {
        struct wl_listener outputs_update;
        struct wl_listener destroy;
    } listeners;
    struct wl_list link;
};

void hrt_border_box_style_ref(struct hrt_border_box_style *style) {
    atomic_fetch_add_explicit(&style->refcount, 1, memory_order_relaxed);
}

static bool draw_box(struct hrt_border_box_style *style,
                     cairo_surface_t *surface, const double width,
                     const double height, const double scale) {
    cairo_t *cairo = cairo_create(surface);
    if (!cairo) {
        wlr_log(WLR_ERROR, "Cannot create cairo surface context for border");
        return false;
    }
    cairo_set_source_rgba(cairo, style->stroke_color[0], style->stroke_color[1],
                          style->stroke_color[2], style->stroke_color[3]);

    const double scaled_line = style->line_width * scale;
    cairo_set_line_width(cairo, scaled_line);
    if (style->border_style == HRT_BORDER_DOTTED) {
        double pattern[] = {8 * scale, 4 * scale};
        cairo_set_dash(cairo, pattern, 2, 0);
    }
    // cairo starts drawing in the middle of the line, so we need to
    // transform the coordinates to make the whole line to be visible
    // within the buffer.
    const double coordinate_offset = scaled_line == 1 ? 1.5 : (scaled_line / 2);
    const double dim_offset = scaled_line == 1 ? 3 : scaled_line;
    cairo_rectangle(cairo, coordinate_offset, coordinate_offset,
                    width - dim_offset, height - dim_offset);
    cairo_stroke(cairo);

    cairo_destroy(cairo);

    return true;
}

struct cairo_buffer *draw_box_surface(struct hrt_border_box_style *style,
                                      int width, int height, float scale) {
    struct cairo_buffer *buffer = cairo_buffer_create(width, height);
    if (!buffer) {
        wlr_log(WLR_ERROR, "%s: cannot get pango layout", __func__);
        return nullptr;
    }

    if (!draw_box(style, buffer->surface, width, height, scale)) {
        wlr_buffer_drop(&buffer->base);
        return nullptr;
    }

    return buffer;
}

static bool set_box_scale(struct hrt_border_box *box, int width, int height,
                          double scale) {
    struct wlr_box border_box;
    if (!compute_scaled_box(width, height, scale, &border_box)) {
        return false;
    }

    enum wlr_scale_filter_mode scale_filter =
        compute_scale_filter(box->scene_buffer->buffer, &border_box, scale);
    wlr_scene_buffer_set_filter_mode(box->scene_buffer, scale_filter);
    wlr_scene_buffer_set_dest_size(box->scene_buffer, width,
                                   height);
    return true;
}

static void hrt_border_box_handle_destroy(struct wl_listener *listener,
                                          void *data) {
    struct hrt_border_box *box =
        wl_container_of(listener, box, listeners.destroy);

    wlr_buffer_drop(&box->buffer->base);
    wl_list_remove(&box->link);
    hrt_border_box_style_unref(box->style);

    wl_list_remove(&box->listeners.destroy.link);
    wl_list_remove(&box->listeners.outputs_update.link);

    free(box);
}

static void hrt_border_box_redraw(struct hrt_border_box *box, int width,
                                  int height);

static void border_box_handle_outputs_update(struct wl_listener *listener,
                                             void *data) {
  struct hrt_border_box *box = wl_container_of(listener, box, listeners.outputs_update);
  // struct wlr_scene_outputs_update_event *event = data;

  const float scale = box->scene_buffer->primary_output->output->scale;
  if (box->scale != scale) {
      box->scale = scale;
      const struct wlr_buffer *const buff = &box->buffer->base;
      set_box_scale(box, buff->width, buff->height, scale);
      hrt_border_box_redraw(box, buff->width, buff->height);
  }
}

static bool buffer_accepts_input_p(struct wlr_scene_buffer *buffer, double *sx,
                                   double *sy) {
    return false;
}

struct hrt_border_box *hrt_border_box_create(struct hrt_scene_layer *parent,
                                             struct hrt_border_box_style *style,
                                             int x, int y, int width,
                                             int height) {
    struct hrt_border_box *box = calloc(1, sizeof(*box));
    if (!box) {
      wlr_log(WLR_ERROR, "Cannot alloc box");
      return nullptr;
    }
    box->buffer = cairo_buffer_create(width, height);
    if (!box->buffer) {
        wlr_log(WLR_ERROR, "Cannot create cairo buffer");
        free(box);
    }

    box->style = style;

    box->scene_buffer =
        wlr_scene_buffer_create(parent->tree, &box->buffer->base);
    if (!box->scene_buffer) {
        wlr_log(WLR_ERROR, "Failed to build wlr_scene_buffer");
        goto error;
    }
    box->scene_buffer->point_accepts_input = buffer_accepts_input_p;

    wlr_scene_node_set_position(&box->scene_buffer->node, x, y);
    const float scale = box->scene_buffer->primary_output->output->scale;
    box->scale        = scale;

    if (!draw_box(style, box->buffer->surface, width, height, scale)) {
        wlr_buffer_drop(&box->buffer->base);
        return nullptr;
    }

    if (!scene_descriptor_assign(&box->scene_buffer->node,
                                 HRT_SCENE_DESC_NON_INTERACTIVE, box)) {
        wlr_log(WLR_ERROR, "Failed to build scene descriptor");
        goto error;
    }

    if (!set_box_scale(box, width, height, scale)) {
        wlr_log(WLR_ERROR, "Invalid scale: %f", scale);
        goto error_scene;
    }

    wlr_scene_node_set_enabled(&box->scene_buffer->node, true);

    box->listeners.destroy.notify = &hrt_border_box_handle_destroy;
    wl_signal_add(&box->scene_buffer->node.events.destroy,
                  &box->listeners.destroy);
    box->listeners.outputs_update.notify = border_box_handle_outputs_update;
    wl_signal_add(&box->scene_buffer->events.outputs_update,
                  &box->listeners.outputs_update);

    wl_list_insert(&style->boxes, &box->link);
    hrt_border_box_style_ref(style);

    return box;

error_scene:
    scene_descriptor_destroy(&box->scene_buffer->node,
                             HRT_SCENE_DESC_NON_INTERACTIVE);
error:
    wlr_buffer_drop(&box->buffer->base);
    free(box);
    return nullptr;
}

void hrt_border_box_destroy(struct hrt_border_box *box) {
    // Freeing the wlr_scene_buffer should trigger this object's destruction:
    wlr_scene_node_destroy(&box->scene_buffer->node);
}

static void hrt_border_box_redraw(struct hrt_border_box *box, int width,
                                  int height) {
  struct cairo_buffer *buffer = draw_box_surface(box->style, width, height, box->scale);
    if (!buffer) {
        wlr_log(WLR_ERROR, "Could not redraw box surface");
        wlr_buffer_drop(&buffer->base);
        return;
    }

    // Keep the old buffer so we can restore it if there's an issue and
    // so we can drop it after we set the new buffer:
    struct cairo_buffer *old = box->buffer;
    wlr_scene_buffer_set_buffer(box->scene_buffer, &buffer->base);
    box->buffer = buffer;

    if (!set_box_scale(box, width, height, box->scale)) {
        wlr_log(WLR_ERROR, "Invalid scale: %f", box->scale);
        // Reset everything back to what it was:
        wlr_scene_buffer_set_buffer(box->scene_buffer, &old->base);
        box->buffer = old;
        wlr_buffer_drop(&buffer->base);
        return;
    }

    wlr_buffer_drop(&old->base);
}

void hrt_border_box_set_size(struct hrt_border_box *box, int width,
                             int height) {
    const struct wlr_buffer *const buff = &box->buffer->base;
    if(buff->width != width || buff->height != height) {
        hrt_border_box_redraw(box, width, height);
    }
}

void hrt_border_box_set_style(struct hrt_border_box *box,
                              struct hrt_border_box_style *style) {
    hrt_border_box_style_unref(box->style);
    hrt_border_box_style_ref(style);
    box->style = style;
    const struct wlr_buffer *const buff = &box->buffer->base;
    hrt_border_box_redraw(box, buff->width,
                          buff->height);
}

void hrt_border_box_set_relative(struct hrt_border_box *box, int x, int y) {
    wlr_scene_node_set_position(&box->scene_buffer->node, x, y);
}

void hrt_border_box_set_enabled(struct hrt_border_box *box, bool enabled) {
    wlr_scene_node_set_enabled(&box->scene_buffer->node, enabled);
}

static void box_style_update(struct hrt_border_box_style *style,
                             enum hrt_border_style border, float color[4],
                             double line_width) {
    style->border_style = border;
    memcpy(style->stroke_color, color, 4 * sizeof(float));
    style->line_width = line_width;
}

struct hrt_border_box_style *
hrt_border_box_style_create(enum hrt_border_style border, float color[4],
                            double line_width) {
    struct hrt_border_box_style *style = calloc(1, sizeof(*style));
    box_style_update(style, border, color, line_width);
    atomic_init(&style->refcount, 1);
    wl_list_init(&style->boxes);

    return style;
}

void hrt_border_box_style_unref(struct hrt_border_box_style *style) {
    if (style == nullptr) {
        return;
    }
    // fetch_sub returns the value BEFORE the subtraction.
    // If the value was 1, it is now 0, and we must free.
    unsigned int count =
        atomic_fetch_sub_explicit(&style->refcount, 1, memory_order_acq_rel);
    if (count == 1) {
        assert(wl_list_empty(&style->boxes) == true);
        free(style);
    }
}

void hrt_border_box_style_update(struct hrt_border_box_style *style,
                                 enum hrt_border_style border, float color[4],
                                 double line_width) {
    box_style_update(style, border, color, line_width);
    struct hrt_border_box *border_box;
    wl_list_for_each(border_box, &style->boxes, link) {
        hrt_border_box_redraw(border_box, border_box->buffer->base.width,
                              border_box->buffer->base.height);
    }
}
