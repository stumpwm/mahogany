#include <stdlib.h>
#include <wayland-server-core.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_idle_inhibit_v1.h>
#include <wlr/types/wlr_idle_notify_v1.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/util/log.h>

#include <hrt/hrt_input.h>
#include <hrt/hrt_server.h>

#include "idle_impl.h"

static struct hrt_server *idle_server;
static struct wlr_idle_notifier_v1 *idle_notifier;
static struct wlr_idle_inhibit_manager_v1 *idle_inhibit;
static struct wl_listener new_idle_inhibitor;
static struct wl_listener idle_inhibit_destroy;
static struct wl_event_source *pending_update;

struct hrt_idle_inhibitor {
    struct wl_listener destroy;
    struct wl_listener surface_map;
    struct wl_listener surface_unmap;
};

struct surface_search {
    struct wlr_surface *surface;
    bool found;
};

static void search_for_surface(struct wlr_scene_buffer *buffer, int sx, int sy,
                               void *data) {
    struct surface_search *search = data;
    struct wlr_scene_surface *scene_surface =
        wlr_scene_surface_try_from_buffer(buffer);

    if (scene_surface && scene_surface->surface == search->surface) {
        search->found = true;
    }
}

static bool surface_is_visible(struct wlr_surface *surface) {
    struct surface_search search = {.surface = surface, .found = false};

    // Traverses enabled nodes, skipping unmapped surfaces and hidden views/groups,
    // anything occluded or off-output still counts as visible.
    wlr_scene_node_for_each_buffer(&idle_server->scene->tree.node,
                                   search_for_surface, &search);
    return search.found;
}

static void idle_inhibit_update(void *data) {
    pending_update                          = NULL;
    bool inhibited                          = false;
    struct wlr_idle_inhibitor_v1 *inhibitor = NULL;

    wl_list_for_each(inhibitor, &idle_inhibit->inhibitors, link) {
        if (surface_is_visible(inhibitor->surface)) {
            inhibited = true;
            break;
        }
    }

    wlr_idle_notifier_v1_set_inhibited(idle_notifier, inhibited);
}

void hrt_idle_inhibit_schedule(void) {
    if (!idle_inhibit || pending_update) {
        return;
    }

    pending_update = wl_event_loop_add_idle(
        wl_display_get_event_loop(idle_server->wl_display), idle_inhibit_update,
        NULL);
}

static void handle_inhibitor_destroy(struct wl_listener *listener, void *data) {
    struct hrt_idle_inhibitor *inhibitor =
        wl_container_of(listener, inhibitor, destroy);

    wl_list_remove(&inhibitor->destroy.link);
    wl_list_remove(&inhibitor->surface_map.link);
    wl_list_remove(&inhibitor->surface_unmap.link);
    free(inhibitor);

    hrt_idle_inhibit_schedule();
}

static void handle_inhibitor_surface_remap(struct wl_listener *listener,
                                           void *data) {
    hrt_idle_inhibit_schedule();
}

static void handle_new_inhibitor(struct wl_listener *listener, void *data) {
    struct wlr_idle_inhibitor_v1 *wlr_inhibitor = data;

    struct hrt_idle_inhibitor *inhibitor = calloc(1, sizeof(*inhibitor));
    if (!inhibitor) {
        wlr_log(WLR_ERROR, "Could not allocate an idle inhibitor");
        return;
    }

    inhibitor->destroy.notify = handle_inhibitor_destroy;
    wl_signal_add(&wlr_inhibitor->events.destroy, &inhibitor->destroy);
    inhibitor->surface_map.notify = handle_inhibitor_surface_remap;
    wl_signal_add(&wlr_inhibitor->surface->events.map, &inhibitor->surface_map);
    inhibitor->surface_unmap.notify = handle_inhibitor_surface_remap;
    wl_signal_add(&wlr_inhibitor->surface->events.unmap,
                  &inhibitor->surface_unmap);

    hrt_idle_inhibit_schedule();
}

static void handle_idle_inhibit_destroy(struct wl_listener *listener,
                                        void *data) {
    wl_list_remove(&new_idle_inhibitor.link);
    wl_list_remove(&idle_inhibit_destroy.link);

    if (pending_update) {
        wl_event_source_remove(pending_update);
        pending_update = NULL;
    }

    idle_inhibit = NULL;
}

bool hrt_idle_init(struct hrt_server *server) {
    idle_server   = server;
    idle_notifier = wlr_idle_notifier_v1_create(server->wl_display);
    idle_inhibit  = wlr_idle_inhibit_v1_create(server->wl_display);
    if (!idle_notifier || !idle_inhibit) {
        wlr_log(WLR_ERROR, "Could not initialize idle handlers");
        return false;
    }

    new_idle_inhibitor.notify = handle_new_inhibitor;
    wl_signal_add(&idle_inhibit->events.new_inhibitor, &new_idle_inhibitor);
    idle_inhibit_destroy.notify = handle_idle_inhibit_destroy;
    wl_signal_add(&idle_inhibit->events.destroy, &idle_inhibit_destroy);

    return true;
}

void hrt_idle_notify_activity(struct hrt_seat *seat) {
    wlr_idle_notifier_v1_notify_activity(idle_notifier, seat->seat);
}
