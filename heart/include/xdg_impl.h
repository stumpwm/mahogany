#pragma once

#include <wayland-server.h>

void handle_new_xdg_surface(struct wl_listener *listener, void *data);
