#ifndef HRT_VIEW_IMPL
#define HRT_VIEW_IMPL

#include <wlr/types/wlr_scene.h>

struct hrt_view;

/**
 * Fully initialize the view and place it in the given scene tree.
 **/
void hrt_view_init(struct hrt_view *view, struct wlr_scene_tree *tree);

/**
 * Cleanup the data initizlized during the hrt_view_init function
 * This is internal, as it doesn't clean up everything and relies on
 * other internal code to completely cleanup after a view.
 */
void hrt_view_cleanup(struct hrt_view *view);

#endif
