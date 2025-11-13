#include <stdbool.h>
#include <wlr/config.h>
#include <wlr/util/log.h>
#include <wlr/backend/wayland.h>
#include <wlr/backend/multi.h>

#if WLR_HAS_X11_BACKEND
#include <wlr/backend/x11.h>
#endif

#include <hrt/hrt_server.h>
#include <hrt/debug/hrt_debug.h>

struct multi_backend_data {
    bool added;
};

static bool add_output(struct wlr_backend *backend) {
    if (wlr_backend_is_wl(backend)) {
        wlr_log(WLR_DEBUG, "Adding WL output");
        return wlr_wl_output_create(backend);
    }

#if WLR_HAS_X11_BACKEND
    if (wlr_backend_is_x11(backend)) {
        wlr_log(WLR_DEBUG, "Adding X11 output");
        return wlr_x11_output_create(backend);
    }
#endif
    return false;
}

static void multi_bakend_callback(struct wlr_backend *backend, void *data) {
    struct multi_backend_data *thing = data;
    if (!thing->added) {
      thing->added = add_output(backend);
    }
}

bool hrt_add_output(const struct hrt_server *server) {
    struct wlr_backend *backend = server->backend;

    if (wlr_backend_is_multi(backend)) {
        struct multi_backend_data data = {
            .added = false,
        };

        wlr_multi_for_each_backend(backend, multi_bakend_callback, &data);
        if (data.added) {
          return true;
        }
    }

    return add_output(backend);
}
