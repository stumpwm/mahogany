#include <wlr/render/pass.h>
#include <wlr/types/wlr_buffer.h>
#include <wlr/util/box.h>

bool compute_scaled_box(int width, int height, double scale,
                        struct wlr_box *box) {
    if (!box || scale <= 0)
        return false;

    *box        = (struct wlr_box){0};
    box->width  = (int)((double)width / scale);
    box->height = (int)((double)height / scale);
    return true;
}

enum wlr_scale_filter_mode compute_scale_filter(struct wlr_buffer *base,
                                                double scale) {
    /* apply nearest scaling if output has an integer scale factor, linear otherwise */
    if (scale < 1) {
        // if we are scaling down, we should always choose linear
        return WLR_SCALE_FILTER_BILINEAR;
    } else {
        // apply nearest scaling if output has an integer scale factor,
        // linear otherwise
        return (ceilf(scale) == scale) ? WLR_SCALE_FILTER_NEAREST :
                                         WLR_SCALE_FILTER_BILINEAR;
    }
}
