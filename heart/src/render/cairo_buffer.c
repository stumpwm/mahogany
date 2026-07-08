#include "render/cairo_buffer.h"
#include <drm_fourcc.h>
#include <stdlib.h>

#include <wlr/interfaces/wlr_buffer.h>

/* this will get called whenever wlr_scene_node_destroy() is called on the parent
 * wlr_scene_buffer, or with wlr_buffer_drop() during error handling */
static void cairo_buffer_destroy(struct wlr_buffer *wlr_buffer) {
    struct hrt_cairo_buffer *buffer = wl_container_of(wlr_buffer, buffer, base);
    wlr_buffer_finish(wlr_buffer);
    if (buffer->surface) {
        cairo_surface_destroy(buffer->surface);
    }
    free(buffer);
}

static bool cairo_buffer_begin_data_ptr_access(struct wlr_buffer *wlr_buffer,
                                               uint32_t flags, void **data,
                                               uint32_t *format,
                                               size_t *stride) {
    struct hrt_cairo_buffer *buffer = wl_container_of(wlr_buffer, buffer, base);
    if (flags & WLR_BUFFER_DATA_PTR_ACCESS_WRITE)
        return false;

    *format = DRM_FORMAT_ARGB8888;
    *data   = cairo_image_surface_get_data(buffer->surface);
    *stride = cairo_image_surface_get_stride(buffer->surface);
    return true;
}

static void cairo_buffer_end_data_ptr_access(struct wlr_buffer *wlr_buffer) {}

static const struct wlr_buffer_impl cairo_buffer_impl = {
    .destroy               = cairo_buffer_destroy,
    .begin_data_ptr_access = cairo_buffer_begin_data_ptr_access,
    .end_data_ptr_access   = cairo_buffer_end_data_ptr_access,
};

struct hrt_cairo_buffer *hrt_cairo_buffer_create(int width, int height) {
    struct hrt_cairo_buffer *buffer = calloc(1, sizeof(*buffer));

    buffer->surface =
        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
    if (cairo_surface_status(buffer->surface) != CAIRO_STATUS_SUCCESS) {
        free(buffer);
        return nullptr;
    }

    wlr_buffer_init(&buffer->base, &cairo_buffer_impl, width, height);
    return buffer;
}
