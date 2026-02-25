#include <hrt/hrt_event_loop.h>
#include <wayland-server-core.h>

struct wl_event_source *hrt_event_loop_add_fd(struct hrt_server *server, int fd,
                                              uint32_t mask,
                                              wl_event_loop_fd_func_t callback,
                                              void *data) {
    struct wl_event_loop *event_loop =
        wl_display_get_event_loop(server->wl_display);
    return wl_event_loop_add_fd(event_loop, fd, mask, callback, data);
}

void hrt_event_loop_remove(struct wl_event_source *source) {
    wl_event_source_remove(source);
}
