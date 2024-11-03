#ifndef HRT_VIEW_IMPL
#define HRT_VIEW_IMPL

struct hrt_view;

/**
 * Cleanup the data initizlized during the hrt_view_init function
 * This is internal, as it doesn't clean up everything and relies on
 * other internal code to completely cleanup after a view.
 */
void hrt_view_cleanup(struct hrt_view *view);

#endif
