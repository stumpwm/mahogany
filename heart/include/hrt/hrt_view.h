#include <wayland-server-core.h>
#include "hrt/hrt_server.h"

struct hrt_view {
	struct wlr_xdg_toplevel *xdg_toplevel;
	struct wlr_scene_tree *scene_tree;
	struct wl_listener map;
	struct wl_listener unmap;
	struct wl_listener destroy;
};
