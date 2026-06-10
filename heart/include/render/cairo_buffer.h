#ifndef RENDER_CAIRO_BUFFER_H
#define RENDER_CAIRO_BUFFER_H

#include <wlr/types/wlr_buffer.h>
#include <cairo.h>

struct cairo_buffer {
    struct wlr_buffer base;
    cairo_surface_t *surface;
};

struct cairo_buffer *cairo_buffer_create(int width, int height);

#endif
