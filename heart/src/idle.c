#include <wlr/types/wlr_idle_notify_v1.h>
#include <wlr/util/log.h>

#include <hrt/hrt_input.h>
#include <hrt/hrt_server.h>

#include "idle_impl.h"

static struct wlr_idle_notifier_v1 *idle_notifier;

bool hrt_idle_init(struct hrt_server *server) {
    idle_notifier = wlr_idle_notifier_v1_create(server->wl_display);
    if (!idle_notifier) {
        wlr_log(WLR_ERROR, "Could not create the idle notifier");
        return false;
    }
    return true;
}

void hrt_idle_notify_activity(struct hrt_seat *seat) {
    wlr_idle_notifier_v1_notify_activity(idle_notifier, seat->seat);
}
