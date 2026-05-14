#include "hrt/hrt_border_box.h"
#include "hrt/hrt_theme.h"
#include "render/scale_util.h"
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

struct hrt_border_box_buffer {
    struct wlr_buffer base;
    cairo_surface_t *surface;
};

struct hrt_border_box {
    struct wlr_scene_buffer *scene_buffer;
    struct hrt_border_box_buffer *buffer;
    struct hrt_border_box_style *style;

    struct {
        struct wl_listener destroy;
    } listeners;
    struct wl_list link;
};

// FIXME: Figure out how to inject the output scale into
//  this code:
constexpr double scale = 2.0;

void hrt_border_box_style_ref(struct hrt_border_box_style *style) {
    atomic_fetch_add_explicit(&style->refcount, 1, memory_order_relaxed);
}

static void box_buffer_destroy(struct wlr_buffer *buffer) {
    struct hrt_border_box_buffer *box_buffer =
        wl_container_of(buffer, box_buffer, base);
    wlr_buffer_finish(buffer);
    cairo_surface_destroy(box_buffer->surface);
    free(box_buffer);
}

static bool box_begin_data_ptr_access(struct wlr_buffer *wlr_buffer,
                                      uint32_t flags, void **data,
                                      uint32_t *format, size_t *stride) {
    struct hrt_border_box_buffer *box_buffer =
        wl_container_of(wlr_buffer, box_buffer, base);
    if (flags & WLR_BUFFER_DATA_PTR_ACCESS_WRITE)
        return false;

    *format = DRM_FORMAT_ARGB8888;
    *data   = cairo_image_surface_get_data(box_buffer->surface);
    *stride = cairo_image_surface_get_stride(box_buffer->surface);
    return true;
}

static void box_end_data_ptr_access(struct wlr_buffer *wlr_buffer) {}

static const struct wlr_buffer_impl box_impl = {
    .destroy               = box_buffer_destroy,
    .begin_data_ptr_access = box_begin_data_ptr_access,
    .end_data_ptr_access   = box_end_data_ptr_access,
};

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
    cairo_rectangle(cairo, 0, 0, width, height);
    cairo_stroke(cairo);

    cairo_destroy(cairo);

    return true;
}

struct hrt_border_box_buffer *
draw_box_surface(struct hrt_border_box_style *style, int width, int height) {
    struct hrt_border_box_buffer *buffer = calloc(1, sizeof(*buffer));

    buffer->surface =
        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
    if (cairo_surface_status(buffer->surface) != CAIRO_STATUS_SUCCESS) {
        cairo_surface_destroy(buffer->surface);
        return nullptr;
    }
    if (!draw_box(style, buffer->surface, width, height, scale)) {
        cairo_surface_destroy(buffer->surface);
        return nullptr;
    }

    wlr_buffer_init(&buffer->base, &box_impl, width, height);
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
    wlr_scene_buffer_set_dest_size(box->scene_buffer, border_box.width,
                                   border_box.height);
    return true;
}

static void hrt_border_box_handle_destroy(struct wl_listener *listener,
                                          void *data) {
    struct hrt_border_box *box =
        wl_container_of(listener, box, listeners.destroy);

    wlr_buffer_drop(&box->buffer->base);
    wl_list_remove(&box->link);
    hrt_border_box_style_unref(box->style);

    wl_list_remove(&listener->link);

    free(box);
}

struct hrt_border_box *hrt_border_box_create(struct hrt_scene_layer *parent,
                                             struct hrt_border_box_style *style,
                                             int x, int y, int width,
                                             int height) {
    struct hrt_border_box *box = calloc(1, sizeof(*box));

    if (!box) {
        return nullptr;
    }
    box->buffer = draw_box_surface(style, width, height);
    struct hrt_border_box_buffer *buffer = box->buffer;
    box->style                           = style;
    if (!buffer) {
        free(box);
        return nullptr;
    }

    box->scene_buffer = wlr_scene_buffer_create(parent->tree, &buffer->base);
    if (!box->scene_buffer) {
        wlr_log(WLR_ERROR, "Failed to build wlr_scene_buffer");
        wlr_buffer_drop(&buffer->base);
        free(box);
        return nullptr;
    }

    if (!set_box_scale(box, width, height, scale)) {
        wlr_log(WLR_ERROR, "Invalid scale: %f", scale);
        wlr_buffer_drop(&buffer->base);
        free(box);
        return nullptr;
    }

    wlr_scene_node_set_enabled(&box->scene_buffer->node, true);
    wlr_scene_node_set_position(&box->scene_buffer->node, x, y);

    // TODO: Scaling!!

    box->listeners.destroy.notify = &hrt_border_box_handle_destroy;
    wl_signal_add(&box->scene_buffer->node.events.destroy,
                  &box->listeners.destroy);

    wl_list_insert(&style->boxes, &box->link);
    hrt_border_box_style_ref(style);

    return box;
}

void hrt_border_box_destroy(struct hrt_border_box *box) {
    // Freeing the wlr_scene_buffer should trigger this object's destruction:
    wlr_scene_node_destroy(&box->scene_buffer->node);
}

static void hrt_border_box_redraw(struct hrt_border_box *box, int width,
                                  int height) {
    struct hrt_border_box_buffer *buffer =
        draw_box_surface(box->style, width, height);
    if (!buffer) {
        wlr_log(WLR_ERROR, "Could not redraw box surface");
        wlr_buffer_drop(&buffer->base);
        return;
    }

    // Do it in this order so we keep the old buffer valid until after it is removed from
    // the scene node. I don't know if it's needed, but it can't hurt:
    struct hrt_border_box_buffer *old = box->buffer;
    wlr_scene_buffer_set_buffer(box->scene_buffer, &buffer->base);
    box->buffer = buffer;

    if (!set_box_scale(box, width, height, scale)) {
        wlr_log(WLR_ERROR, "Invalid scale: %f", scale);
        // Reset everything back to what it was:
        wlr_scene_buffer_set_buffer(box->scene_buffer, &old->base);
        box->buffer = old;
        wlr_buffer_drop(&buffer->base);
    }
}

void hrt_border_box_set_size(struct hrt_border_box *box, int width,
                             int height) {
    hrt_border_box_redraw(box, width, height);
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
    memcpy(style->stroke_color, color, 4 * sizeof(double));
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
