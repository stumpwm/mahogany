#include <wlr/render/pass.h>
#include <wlr/types/wlr_buffer.h>
#include <wlr/util/box.h>

bool compute_scaled_box(
    int width, int height, double scale, struct wlr_box *box
) {
    if (!box || scale <= 0)
        return false;

    *box        = (struct wlr_box){0};
    box->width  = (int)((double)width / scale);
    box->height = (int)((double)height / scale);
    return true;
}

enum wlr_scale_filter_mode compute_scale_filter(
    struct wlr_buffer *base, struct wlr_box *message_box, double scale
) {
    /* apply nearest scaling if output has an integer scale factor, linear otherwise */
    enum wlr_scale_filter_mode scale_filter = (ceilf(scale) == scale) ?
        WLR_SCALE_FILTER_NEAREST :
        WLR_SCALE_FILTER_BILINEAR;
    if (message_box->width < base->width && message_box->height < base->height)
        /* if we are scaling down, we should always choose linear */
        scale_filter = WLR_SCALE_FILTER_BILINEAR;
    return scale_filter;
}
