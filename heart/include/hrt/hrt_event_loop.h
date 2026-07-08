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

struct hrt_fd_semaphore {
    int fd;
    struct wl_event_source *event_source;
};

/**
 * Create a semaphore fd (via eventfd(2)), and call the provided function
 * whenever the semaphore is non-zero.
 **/
struct hrt_fd_semaphore *
hrt_event_loop_semaphore_add(struct hrt_server *server, int initval,
                             hrt_event_loop_fd_func_t callback);

/**
 * Increment the given semaphore by the given value.
 **/
bool hrt_event_loop_semaphore_increment(struct hrt_fd_semaphore *fd,
                                        uint64_t increment);

/**
 * Decrement the semaphore by 1
 **/
bool hrt_event_loop_semaphore_decrement(struct hrt_fd_semaphore *fd);

/**
 * Close the semaphore fd and remove it from the event loop.
 **/
void hrt_event_loop_semaphore_close(struct hrt_fd_semaphore *fd);

#endif
