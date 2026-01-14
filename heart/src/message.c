#include <cairo/cairo.h>
#include <pango/pangocairo.h>
#include <drm_fourcc.h>

#include "wlr/interfaces/wlr_buffer.h"
#include "wlr/util/log.h"

#include <hrt/hrt_message.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_scene.h>
#include <hrt/hrt_server.h>

#define DEFAULT_FONT            "monospace 15"
#define DEFAULT_BORDER_PADDING  5
#define DEFAULT_BORDER_WIDTH    1

struct message {
    struct wlr_buffer base;
    cairo_surface_t *surface;
};

/* this will get called whenever wlr_scene_node_destroy() is called on the parent
 * wlr_scene_buffer, or with wlr_buffer_drop() during error handling */
static void message_destroy(struct wlr_buffer *wlr_buffer) {
    struct message *message = wl_container_of(wlr_buffer, message, base);
    wlr_buffer_finish(wlr_buffer);
    if (message->surface)
        cairo_surface_destroy(message->surface);
    free(message);
}

static bool message_begin_data_ptr_access(struct wlr_buffer *wlr_buffer,
        uint32_t flags, void **data, uint32_t *format, size_t *stride) {
    struct message *buffer = wl_container_of(wlr_buffer, buffer, base);
    if (flags & WLR_BUFFER_DATA_PTR_ACCESS_WRITE)
        return false;

    *format = DRM_FORMAT_ARGB8888;
    *data = cairo_image_surface_get_data(buffer->surface);
    *stride = cairo_image_surface_get_stride(buffer->surface);
    return true;
}

static void message_end_data_ptr_access(struct wlr_buffer *wlr_buffer) {}

static const struct wlr_buffer_impl message_impl = {
    .destroy = message_destroy,
    .begin_data_ptr_access = message_begin_data_ptr_access,
    .end_data_ptr_access = message_end_data_ptr_access
};

static PangoLayout *get_pango_layout(PangoContext *context, const char *font, const char *text) {
    PangoLayout *layout = pango_layout_new(context);
    PangoFontDescription *desc = pango_font_description_from_string(font);

    pango_layout_set_font_description(layout, desc);
    pango_layout_set_single_paragraph_mode(layout, false);
    pango_layout_set_text(layout, text, -1);

    pango_font_description_free(desc);
    return layout;
}

/* TODO: tweak font options */
static cairo_font_options_t *get_font_options() {
    cairo_font_options_t *font_options = cairo_font_options_create();
    cairo_font_options_set_hint_style(font_options, CAIRO_HINT_STYLE_FULL);
    cairo_font_options_set_antialias(font_options, CAIRO_ANTIALIAS_SUBPIXEL);
    cairo_font_options_set_subpixel_order(font_options, CAIRO_SUBPIXEL_ORDER_RGB);

    return font_options;
}

static struct message *render_message(const char *text, double scale) {
    /* TODO: configurable options: font (name/size), bg/fg color, border, per-output scale */
    char *font = DEFAULT_FONT;
    int border_padding = DEFAULT_BORDER_PADDING;
    int border_width = DEFAULT_BORDER_WIDTH;

    struct message *message = NULL;
    PangoContext *pango_context = NULL;
    cairo_font_options_t *font_options = NULL;
    PangoLayout *pango_layout = NULL;

    /* setup pango layout and measure rendered text size */
    pango_context = pango_font_map_create_context(pango_cairo_font_map_get_default());
    font_options = get_font_options();
    pango_cairo_context_set_font_options(pango_context, font_options);

    pango_layout = get_pango_layout(pango_context, font, text);
    if (!pango_layout) {
        wlr_log(WLR_ERROR, "%s: cannot get pango layout", __func__);
        goto out;
    }

    int text_width, text_height;
    pango_layout_get_pixel_size(pango_layout, &text_width, &text_height);

    /* add border dimensions */
    int total_width = text_width + ((border_width + border_padding) * 2);
    int total_height = text_height + ((border_width + border_padding) * 2);

    /* scale to specific output's dpi.
     * it's probably better to scale at the source with proper font antialiasing instead of letting
     * the compositor scale the whole surface when rendering on a specific output */
    int scaled_width = (int)(total_width * scale);
    int scaled_height = (int)(total_height * scale);

    message = calloc(1, sizeof(*message));
    if (!message) {
        wlr_log(WLR_ERROR, "%s: cannot allocate message: %s", __func__, strerror(errno));
        goto out;
    }

    /* render the text in a cairo surface */
    message->surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, scaled_width, scaled_height);
    if (cairo_surface_status(message->surface) != CAIRO_STATUS_SUCCESS) {
        free(message);
        message = NULL;
        goto out;
    }

    cairo_t *c = cairo_create(message->surface);
    if (!c) {
        cairo_surface_destroy(message->surface);
        free(message);
        message = NULL;
        goto out;
    }

    cairo_set_antialias(c, CAIRO_ANTIALIAS_BEST);
    cairo_set_font_options(c, font_options);
    cairo_scale(c, scale, scale);

    /* clear background */
    cairo_set_operator(c, CAIRO_OPERATOR_SOURCE);
    cairo_set_source_rgba(c, 0, 0, 0,0);            /* TODO bg color */
    cairo_paint(c);

    /* draw border */
    cairo_set_operator(c, CAIRO_OPERATOR_OVER);
    cairo_set_source_rgba(c, 1.0, 1.0, 1.0, 1.0);   /* TODO border color */
    cairo_set_line_width(c, border_width);
    double inset = border_width / 2.0;
    cairo_rectangle(c, inset, inset, total_width - border_width, total_height - border_width);
    cairo_stroke(c);

    /* draw text */
    cairo_set_source_rgba(c, 1.0, 1.0, 1.0, 1.0);   /* TODO fg color */
    cairo_move_to(c, border_width + border_padding, border_width + border_padding);
    pango_cairo_update_layout(c, pango_layout);
    pango_cairo_show_layout(c, pango_layout);
    cairo_destroy(c);

    /* wrap it in the wlr_buffer that will be passed to wlr_scene_buffer_create */
    wlr_buffer_init(&message->base, &message_impl, scaled_width, scaled_height);

out:
    if (pango_layout)
        g_object_unref(pango_layout);
    if (pango_context)
        g_object_unref(pango_context);
    if (font_options)
        cairo_font_options_destroy(font_options);

    return message;
}

static bool gravity_coords(enum window_gravity gravity,
                           int width, int height,
                           int x, int y,
                           int min_x, int min_y,
                           int max_x, int max_y,
                           int *pos_x, int *pos_y) {
    if (!pos_x || !pos_y ||
        gravity < GRAVITY_TOP_RIGHT || gravity > GRAVITY_MAX ||
        width < 0 || height < 0 || min_x > max_x || min_y > max_y)
        return false;

    int container_width = max_x - min_x;
    int container_height = max_y - min_y;

    if (width > container_width || height > container_height)
        return false;

    switch (gravity) {
        case GRAVITY_TOP_LEFT:
        case GRAVITY_BOTTOM_LEFT:
        case GRAVITY_LEFT:
            x += min_x;
            break;

        case GRAVITY_TOP_RIGHT:
        case GRAVITY_BOTTOM_RIGHT:
        case GRAVITY_RIGHT:
            x += max_x - width;
            break;

        case GRAVITY_TOP:
        case GRAVITY_BOTTOM:
        case GRAVITY_CENTER:
            x += min_x + (container_width - width) / 2;
            break;

        default:
            return false;
    };

    switch (gravity) {
        case GRAVITY_TOP_LEFT:
        case GRAVITY_TOP_RIGHT:
        case GRAVITY_TOP:
            y += min_y;
            break;

        case GRAVITY_BOTTOM_LEFT:
        case GRAVITY_BOTTOM_RIGHT:
        case GRAVITY_BOTTOM:
            y += max_y - height;
            break;

        case GRAVITY_LEFT:
        case GRAVITY_RIGHT:
        case GRAVITY_CENTER:
            y += min_y + (container_height - height) / 2;
            break;

        default:
            return false;
    }

    wlr_log(WLR_DEBUG, "%s: gravity=%u, size=(%d, %d), bounds=(%d, %d, %d, %d) -> (%d, %d)",
            __func__, gravity, width, height, min_x, min_y, max_x, max_y, x, y);

    *pos_x = x;
    *pos_y = y;

    return true;
}

bool hrt_toast_message(struct hrt_server *server,
                       struct hrt_output *output,
                       const char *text,
                       enum window_gravity gravity,
                       int margin_x,
                       int margin_y,
                       int ms_delay) {
    /* cancel any previously running timeout */
    wl_event_source_timer_update(server->message_timer_source, 0);

    /* clear previous message if it's still being displayed */
    if (server->message_buffer) {
        wlr_scene_node_destroy(&server->message_buffer->node);
        server->message_buffer = NULL;
    }

    double scale = output->wlr_output->scale;
    struct message *message = render_message(text, scale);
    if (!message)
        return false;

    struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_create(server->scene_root->overlay, &message->base);
    if (!scene_buffer) {
        wlr_buffer_drop(&message->base);
        return false;
    }

    int x = 0;
    int y = 0;

    if (!gravity_coords(gravity,
                        scene_buffer->buffer->width,
                        scene_buffer->buffer->height,
                        output->wlr_scene->x,
                        output->wlr_scene->y,
                        margin_x * 2, margin_y * 2,
                        output->wlr_output->width - (margin_x * 2),
                        output->wlr_output->height - (margin_y * 2),
                        &x, &y)) {
        wlr_scene_node_destroy(&server->message_buffer->node);
        return false;
    }


    wlr_scene_node_set_position(&scene_buffer->node, x, y);
    wlr_scene_node_set_enabled(&scene_buffer->node, true);

    server->message_buffer = scene_buffer;
    if (wl_event_source_timer_update(server->message_timer_source, ms_delay) != 0) {
        wlr_scene_node_destroy(&server->message_buffer->node);
        server->message_buffer = NULL;
        return false;
    } else
        return true;
}

static int close_message_callback(void *data) {
    struct hrt_server *server = (struct hrt_server *)data;
    if (server->message_buffer) {
        wlr_scene_node_destroy(&server->message_buffer->node);
        server->message_buffer = NULL;
    }
    return 0;
}

bool hrt_message_init(struct hrt_server *server) {
    server->message_buffer = NULL;

    struct wl_event_loop *event_loop = wl_display_get_event_loop(server->wl_display);
    if (!event_loop)
        return false;

    server->message_timer_source = wl_event_loop_add_timer(event_loop, close_message_callback, server);
    return (server->message_timer_source != NULL);
}

void hrt_message_destroy(struct hrt_server *server) {
    if (server->message_timer_source) {
        wl_event_source_remove(server->message_timer_source);
        server->message_timer_source = NULL;
    }
    close_message_callback((void *)server);
}
