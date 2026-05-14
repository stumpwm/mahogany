#ifndef BORDER_BOX
#define BORDER_BOX

#include "hrt/hrt_scene.h"
#include "hrt/hrt_theme.h"
#include <wlr/types/wlr_scene.h>

struct hrt_border_box_style;
struct hrt_border_box;

struct hrt_border_box_style *
hrt_border_box_style_create(enum hrt_border_style border, float color[4],
                        double line_width);

void hrt_border_box_style_ref(struct hrt_border_box_style *style);
void hrt_border_box_style_unref(struct hrt_border_box_style *);

void hrt_border_box_style_update(struct hrt_border_box_style *style,
                                 enum hrt_border_style border, float color[4],
                                 double line_width);

struct hrt_border_box *hrt_border_box_create(struct hrt_scene_layer *parent,
                                             struct hrt_border_box_style *style,
                                             int x, int y, int width,
                                             int height);

void hrt_border_box_destroy(struct hrt_border_box *box);

void hrt_border_box_set_size(struct hrt_border_box *box, int width, int height);

void hrt_border_box_set_relative(struct hrt_border_box *box, int x, int y);

void hrt_border_box_set_enabled(struct hrt_border_box *box, bool enabled);

#endif
