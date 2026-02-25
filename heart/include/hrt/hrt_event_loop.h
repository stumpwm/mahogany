#ifndef HRT_EVENT_LOOP_H
#define HRT_EVENT_LOOP_H

#include <stdint.h>
#include <wayland-server-core.h>

#include <hrt/hrt_server.h>

enum hrt_event_mask {
    HRT_EVENT_READABLE = WL_EVENT_READABLE,
    HRT_EVENT_WRITABLE = WL_EVENT_WRITABLE,
    HRT_EVENT_HANGUP   = WL_EVENT_HANGUP,
    HRT_EVENT_ERROR    = WL_EVENT_ERROR
};

typedef wl_event_loop_fd_func_t hrt_event_loop_fd_func_t;

struct wl_event_source *hrt_event_loop_add_fd(struct hrt_server *server, int fd,
                                              uint32_t mask,
                                              hrt_event_loop_fd_func_t callback,
                                              void *data);

void hrt_event_loop_remove(struct wl_event_source *source);

#endif
