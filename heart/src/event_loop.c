#include "wlr/util/log.h"
#include <asm-generic/errno-base.h>
#include <errno.h>
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/eventfd.h>

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

struct hrt_fd_semaphore *
hrt_event_loop_semaphore_add(struct hrt_server *server, int initval,
                             hrt_event_loop_fd_func_t callback) {
    struct hrt_fd_semaphore *semaphore = calloc(1, sizeof(&semaphore));
    if (!semaphore) {
        return nullptr;
    }
    int fd        = eventfd(initval, EFD_NONBLOCK | EFD_SEMAPHORE);
    semaphore->fd = fd;
    if (fd == 0) {
        free(semaphore);
        return nullptr;
    }
    semaphore->event_source = hrt_event_loop_add_fd(
        server, fd, WL_EVENT_READABLE, callback, nullptr);

    if (!semaphore->event_source) {
        close(fd);
        free(semaphore);
        return nullptr;
    }
    return semaphore;
}

bool hrt_event_loop_semaphore_increment(struct hrt_fd_semaphore *fd,
                                        uint64_t increment) {
    uint64_t buffer[1] = {increment};
    if (!write(fd->fd, buffer, sizeof(buffer))) {
        if (errno == EAGAIN) {
            wlr_log(WLR_ERROR,
                    "Incrementing semaphore failed: counter too big.");
        } else {
            wlr_log(WLR_ERROR, "Cannot read semaphore");
        }
        return false;
    }
    return true;
}

bool hrt_event_loop_semaphore_decrement(struct hrt_fd_semaphore *fd) {
    uint64_t buffer[1];
    if (!read(fd->fd, buffer, sizeof(buffer))) {
        if (errno == EAGAIN) {
            wlr_log(WLR_ERROR, "Semaphore is zero when reading");
        } else {
            wlr_log(WLR_ERROR, "Cannot read semahore");
        }
        return false;
    }
    return true;
}

void hrt_event_loop_semaphore_close(struct hrt_fd_semaphore *fd) {
    hrt_event_loop_remove(fd->event_source);
    close(fd->fd);
    free(fd);
}
