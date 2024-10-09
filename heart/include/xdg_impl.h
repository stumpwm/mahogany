#pragma once

#include <wayland-server-core.h>
#include <wayland-server.h>
#include "hrt/hrt_server.h"

struct hrt_xdg_popup {
	struct wlr_xdg_popup *xdg_popup;
	struct wl_listener commit;
	struct wl_listener destroy;
};

bool hrt_xdg_shell_init(struct hrt_server *server);
