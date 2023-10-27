#include <stdint.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/types/wlr_scene.h>

#include "hrt/hrt_view.h"

void hrt_view_init(struct hrt_view *view, struct wlr_scene_tree *tree) {
	view->scene_tree = wlr_scene_tree_create(tree);

	struct wlr_scene_tree *xdg_tree =
		wlr_scene_xdg_surface_create(view->scene_tree, view->xdg_toplevel->base);
	xdg_tree->node.data = view;
	view->xdg_surface->data = xdg_tree;
}

uint32_t hrt_view_set_size(struct hrt_view *view, int width, int height) {
	return wlr_xdg_toplevel_set_size(view->xdg_toplevel, width, height);
}

void hrt_view_set_relative(struct hrt_view *view, int x, int y) {
	wlr_scene_node_set_position(&view->scene_tree->node, x, y);
}
