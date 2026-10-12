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

struct hrt_idle_inhibitor {
    struct hrt_server *server;
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

static bool surface_is_visible(struct hrt_server *server,
                               struct wlr_surface *surface) {
    struct surface_search search = {.surface = surface, .found = false};

    // Traverses enabled nodes, skipping unmapped surfaces and hidden views/groups,
    // anything occluded or off-output still counts as visible.
    wlr_scene_node_for_each_buffer(&server->scene->tree.node,
                                   search_for_surface, &search);
    return search.found;
}

static void idle_inhibit_update(void *data) {
    struct hrt_server *server                   = data;
    server->idle_inhibit_scheduled_update       = nullptr;
    bool inhibited                              = false;
    struct wlr_idle_inhibitor_v1 *wlr_inhibitor = nullptr;

    wl_list_for_each(wlr_inhibitor, &server->idle_inhibit_manager->inhibitors,
                     link) {
        if (surface_is_visible(server, wlr_inhibitor->surface)) {
            inhibited = true;
            break;
        }
    }

    wlr_idle_notifier_v1_set_inhibited(server->idle_notifier, inhibited);
}

void hrt_idle_inhibit_schedule(struct hrt_server *server) {
    if (!server->idle_inhibit_manager ||
        server->idle_inhibit_scheduled_update) {
        return;
    }

    server->idle_inhibit_scheduled_update =
        wl_event_loop_add_idle(wl_display_get_event_loop(server->wl_display),
                               idle_inhibit_update, server);
}

static void handle_inhibitor_destroy(struct wl_listener *listener, void *data) {
    struct hrt_idle_inhibitor *inhibitor =
        wl_container_of(listener, inhibitor, destroy);
    struct hrt_server *server = inhibitor->server;

    wl_list_remove(&inhibitor->destroy.link);
    wl_list_remove(&inhibitor->surface_map.link);
    wl_list_remove(&inhibitor->surface_unmap.link);
    free(inhibitor);

    hrt_idle_inhibit_schedule(server);
}

static void handle_inhibitor_surface_map(struct wl_listener *listener,
                                         void *data) {
    struct hrt_idle_inhibitor *inhibitor =
        wl_container_of(listener, inhibitor, surface_map);
    hrt_idle_inhibit_schedule(inhibitor->server);
}

static void handle_inhibitor_surface_unmap(struct wl_listener *listener,
                                           void *data) {
    struct hrt_idle_inhibitor *inhibitor =
        wl_container_of(listener, inhibitor, surface_unmap);
    hrt_idle_inhibit_schedule(inhibitor->server);
}

static void handle_new_inhibitor(struct wl_listener *listener, void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, new_idle_inhibitor);
    struct wlr_idle_inhibitor_v1 *wlr_inhibitor = data;

    struct hrt_idle_inhibitor *inhibitor = calloc(1, sizeof(*inhibitor));
    if (!inhibitor) {
        wlr_log(WLR_ERROR, "Could not allocate an idle inhibitor");
        return;
    }

    inhibitor->server         = server;
    inhibitor->destroy.notify = handle_inhibitor_destroy;
    wl_signal_add(&wlr_inhibitor->events.destroy, &inhibitor->destroy);
    inhibitor->surface_map.notify = handle_inhibitor_surface_map;
    wl_signal_add(&wlr_inhibitor->surface->events.map, &inhibitor->surface_map);
    inhibitor->surface_unmap.notify = handle_inhibitor_surface_unmap;
    wl_signal_add(&wlr_inhibitor->surface->events.unmap,
                  &inhibitor->surface_unmap);

    hrt_idle_inhibit_schedule(server);
}

static void handle_inhibit_manager_destroy(struct wl_listener *listener,
                                           void *data) {
    struct hrt_server *server = wl_container_of(
        listener, server, destroy_listener.idle_inhibit_manager);

    wl_list_remove(&server->new_idle_inhibitor.link);
    wl_list_remove(&server->destroy_listener.idle_inhibit_manager.link);

    if (server->idle_inhibit_scheduled_update) {
        wl_event_source_remove(server->idle_inhibit_scheduled_update);
        server->idle_inhibit_scheduled_update = nullptr;
    }

    server->idle_inhibit_manager = nullptr;
}

bool hrt_idle_init(struct hrt_server *server) {
    server->idle_notifier = wlr_idle_notifier_v1_create(server->wl_display);
    server->idle_inhibit_manager =
        wlr_idle_inhibit_v1_create(server->wl_display);
    if (!server->idle_notifier || !server->idle_inhibit_manager) {
        wlr_log(WLR_ERROR, "Could not initialize idle handlers");
        return false;
    }

    server->new_idle_inhibitor.notify = handle_new_inhibitor;
    wl_signal_add(&server->idle_inhibit_manager->events.new_inhibitor,
                  &server->new_idle_inhibitor);
    server->destroy_listener.idle_inhibit_manager.notify =
        handle_inhibit_manager_destroy;
    wl_signal_add(&server->idle_inhibit_manager->events.destroy,
                  &server->destroy_listener.idle_inhibit_manager);

    return true;
}

void hrt_idle_notify_activity(struct hrt_seat *seat) {
    wlr_idle_notifier_v1_notify_activity(seat->server->idle_notifier,
                                         seat->seat);
}
