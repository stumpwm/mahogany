#include <cairo/cairo.h>
#include <pango/pangocairo.h>
#include <drm_fourcc.h>

#include "wlr/interfaces/wlr_buffer.h"
#include "wlr/util/log.h"

#include <hrt/hrt_message.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_scene.h>
#include <hrt/hrt_server.h>

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

static PangoLayout *get_pango_layout(PangoContext *context, const char *font, double scale, const char *text) {
    PangoLayout *layout = pango_layout_new(context);
    PangoFontDescription *desc = pango_font_description_from_string(font);
    PangoAttrList *attrs;

    /* scale for specific output
     * it's better to scale at the font rendering level with proper antialiasing instead of letting
     * the compositor scale the whole surface when rendering on a specific output,
     * which results in blurry rendered text */
    attrs = pango_attr_list_new();
    pango_attr_list_insert(attrs, pango_attr_scale_new(scale));

    pango_layout_set_font_description(layout, desc);
    pango_layout_set_single_paragraph_mode(layout, false);
    pango_layout_set_attributes(layout, attrs);
    pango_layout_set_text(layout, text, -1);

    pango_attr_list_unref(attrs);
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

static struct message *render_message(const char *text, double scale,
                                      struct hrt_message_theme *theme) {
    /* TODO: configurable options: font (name/size), bg/fg color, border, per-output scale */
    char *font         = theme->font;
    int border_padding = theme->message_padding;
    int border_width   = theme->message_border_width;

    struct message *message = NULL;
    PangoContext *pango_context = NULL;
    cairo_font_options_t *font_options = NULL;
    PangoLayout *pango_layout = NULL;

    /* setup pango layout and measure rendered text size */
    pango_context = pango_font_map_create_context(pango_cairo_font_map_get_default());
    font_options = get_font_options();
    pango_cairo_context_set_font_options(pango_context, font_options);

    pango_layout = get_pango_layout(pango_context, font, scale, text);
    if (!pango_layout) {
        wlr_log(WLR_ERROR, "%s: cannot get pango layout", __func__);
        goto out;
    }

    int text_width, text_height;
    pango_layout_get_pixel_size(pango_layout, &text_width, &text_height);

    /* add border dimensions */
    int total_width = text_width + ((border_width + border_padding) * 2);
    int total_height = text_height + ((border_width + border_padding) * 2);

    message = calloc(1, sizeof(*message));
    if (!message) {
        wlr_log(WLR_ERROR, "%s: cannot allocate message: %s", __func__, strerror(errno));
        goto out;
    }

    /* render the text in a cairo surface */
    message->surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, total_width, total_height);
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

    /* clear background */
    cairo_set_operator(c, CAIRO_OPERATOR_SOURCE);
    cairo_set_source_rgba(c, theme->background_color[0],
                          theme->background_color[1],
                          theme->background_color[2],
                          theme->background_color[3]);
    cairo_paint(c);

    /* draw border */
    cairo_set_operator(c, CAIRO_OPERATOR_OVER);
    cairo_set_source_rgba(c, theme->border_color[0],
                          theme->border_color[1],
                          theme->border_color[2],
                          theme->border_color[3]);
    cairo_set_line_width(c, border_width);
    double inset = border_width / 2.0;
    cairo_rectangle(c, inset, inset, total_width - border_width,
                    total_height - border_width);
    cairo_stroke(c);

    /* draw text */
    cairo_set_source_rgba(c, theme->font_color[0],
                          theme->font_color[1],
                          theme->font_color[2],
                          theme->font_color[3]);
    cairo_move_to(c, border_width + border_padding,
                  border_width + border_padding);
    pango_cairo_update_layout(c, pango_layout);
    pango_cairo_show_layout(c, pango_layout);
    cairo_destroy(c);

    /* wrap it in the wlr_buffer that will be passed to wlr_scene_buffer_create */
    wlr_buffer_init(&message->base, &message_impl, total_width, total_height);

out:
    if (pango_layout)
        g_object_unref(pango_layout);
    if (pango_context)
        g_object_unref(pango_context);
    if (font_options)
        cairo_font_options_destroy(font_options);

    return message;
}

static bool scaled_box(int width, int height, double scale, struct wlr_box *box) {
    if (!box || scale <= 0)
        return false;

    *box = (struct wlr_box){0};
    box->width = (int)((double)width / scale);
    box->height = (int)((double)height / scale);
    return true;
}

static bool margin_box(struct wlr_box *src,
                       int margin_x,
                       int margin_y,
                       double scale,
                       struct wlr_box *dst) {
    if (!src || !dst)
        return false;

    /* src is assumed to be already scaled by wlr_output_layout_get_box
     * but we need to manually scale the margins */
    double fmargin_x = (double)margin_x / scale;
    double fmargin_y = (double)margin_y / scale;

    if ((int)fmargin_x > src->width || (int)fmargin_y > src->height)
        return false;

    dst->x = src->x + (int)fmargin_x;
    dst->y = src->y + (int)fmargin_y;
    dst->width = src->width - (int)(fmargin_x * 2);
    dst->height = src->height - (int)(fmargin_y * 2);
    return true;
}

/* completes message_box's x/y coordinates for gravity placement within output_box */
static bool gravity_coords(enum window_gravity gravity,
                           struct wlr_box *output_box,
                           struct wlr_box *message_box) {
    if (!message_box || !output_box ||
        gravity < GRAVITY_TOP_RIGHT || gravity > GRAVITY_MAX) {
        wlr_log(WLR_ERROR, "%s: invalid parameter", __func__);
        return false;
    }

    wlr_log(WLR_DEBUG, "gravity=%u, message_box=(x=%d, y=%d, w=%d, h=%d), output_box=(x=%d, y=%d, w=%d, h=%d)",
            gravity,
            message_box->x, message_box->y, message_box->width, message_box->height,
            output_box->x, output_box->y, output_box->width, output_box ->height);

    if (!wlr_box_contains_box(output_box, message_box)) {
        wlr_log(WLR_ERROR, "%s: message box out of bounds", __func__);
        return false;
    }

    switch (gravity) {
        case GRAVITY_TOP_LEFT:
        case GRAVITY_BOTTOM_LEFT:
        case GRAVITY_LEFT:
            message_box->x = output_box->x;
            break;

        case GRAVITY_TOP_RIGHT:
        case GRAVITY_BOTTOM_RIGHT:
        case GRAVITY_RIGHT:
            message_box->x = output_box->x + (output_box->width - message_box->width);
            break;

        case GRAVITY_TOP:
        case GRAVITY_BOTTOM:
        case GRAVITY_CENTER:
            message_box->x = output_box->x + (output_box->width - message_box->width) / 2;
            break;

        default:
            break;
    };

    switch (gravity) {
        case GRAVITY_TOP_LEFT:
        case GRAVITY_TOP_RIGHT:
        case GRAVITY_TOP:
            message_box->y = output_box->y;
            break;

        case GRAVITY_BOTTOM_LEFT:
        case GRAVITY_BOTTOM_RIGHT:
        case GRAVITY_BOTTOM:
            message_box->y = output_box->y + (output_box->height - message_box->height);
            break;

        case GRAVITY_LEFT:
        case GRAVITY_RIGHT:
        case GRAVITY_CENTER:
            message_box->y = output_box->y + (output_box->height - message_box->height) / 2;
            break;

        default:
            break;
    }

    wlr_log(WLR_DEBUG, "-> coords=(x=%d, y=%d)", message_box->x, message_box->y);

    return true;
}

bool hrt_toast_message(struct hrt_server *server, struct hrt_output *output,
                       const char *text, enum window_gravity gravity,
                       struct hrt_message_theme *theme, int ms_delay) {
    /* cancel any previously running timeout */
    wl_event_source_timer_update(server->message_timer_source, 0);

    /* clear previous message if it's still being displayed */
    if (server->message_buffer) {
        wlr_scene_node_destroy(&server->message_buffer->node);
        server->message_buffer = NULL;
    }

    struct wlr_box output_box;
    wlr_output_layout_get_box(server->output_layout, output->wlr_output, &output_box);
    wlr_log(WLR_DEBUG, "output_box=(x=%d, y=%d, w=%d, h=%d)",
            output_box.x, output_box.y, output_box.width, output_box.height);

    double scale = output->wlr_output->scale;
    struct wlr_box framed_box;
    if (!margin_box(&output_box, theme->margin_x, theme->margin_y, scale, &framed_box))
        return false;

    struct message *message = render_message(text, scale, theme);
    if (!message)
        return false;

    struct wlr_box message_box;
    if (!scaled_box(message->base.width, message->base.height, scale, &message_box)) {
        wlr_buffer_drop(&message->base);
        return false;
    }

    /* set message_box's initial position to the same as framed_box in order to use
     * wlr_box_contains_box() for bounds checking.
     * this is already scaled in framed_box so we don't need to adjust it */
    message_box.x = framed_box.x;
    message_box.y = framed_box.y;

    if (!gravity_coords(gravity,
                        &framed_box,
                        &message_box)) {
        wlr_buffer_drop(&message->base);
        return false;
    }

    struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_create(server->scene_root->overlay, &message->base);
    if (!scene_buffer) {
        wlr_buffer_drop(&message->base);
        return false;
    }

    /* apply nearest scaling if output has an integer scale factor, linear otherwise */
    enum wlr_scale_filter_mode scale_filter = (ceilf(scale) == scale) ?
        WLR_SCALE_FILTER_NEAREST :
        WLR_SCALE_FILTER_BILINEAR;
    if (message_box.width < message->base.width &&
        message_box.height < message->base.height)
        /* if we are scaling down, we should always choose linear */
        scale_filter = WLR_SCALE_FILTER_BILINEAR;

    wlr_scene_buffer_set_filter_mode(scene_buffer, scale_filter);
    wlr_scene_buffer_set_dest_size(scene_buffer, message_box.width, message_box.height);

    wlr_scene_node_set_position(&scene_buffer->node, message_box.x, message_box.y);
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
