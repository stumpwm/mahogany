#include "hrt/hrt_server.h"
#include "wlr/util/log.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#include <wayland-server-core.h>
#include <wayland-util.h>
#include <wlr/render/wlr_renderer.h>

#include <hrt/hrt_output.h>

static void handle_request_state(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "Request State Handled");
    struct hrt_output *output =
        wl_container_of(listener, output, request_state);
    const struct wlr_output_event_request_state *event = data;
    wlr_output_commit_state(output->wlr_output, event->state);
}

static void handle_frame_notify(struct wl_listener *listener, void *data) {
    struct hrt_output *output = wl_container_of(listener, output, frame);
    struct wlr_scene *scene   = output->server->scene;

    struct wlr_scene_output *scene_output =
        wlr_scene_get_scene_output(scene, output->wlr_output);
    wlr_scene_output_commit(scene_output, NULL);

    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    wlr_scene_output_send_frame_done(scene_output, &now);
}

static void handle_output_destroy(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "Output destroyed");
    struct hrt_output *output = wl_container_of(listener, output, destroy);
    struct hrt_server *server = output->server;
    server->output_callback->output_removed(output);

    wl_list_remove(&output->frame.link);
    wl_list_remove(&output->request_state.link);
    wl_list_remove(&output->destroy.link);

    // wlr_output_layout removes the output by itself.

    free(output);
}

// temp random float generator
static float float_rand() {
    return (float)(rand() / (double)RAND_MAX); /* [0, 1.0] */
}

static struct hrt_output *hrt_output_create(struct hrt_server *server,
                                            struct wlr_output *wlr_output) {
    struct hrt_output *output = calloc(1, sizeof(struct hrt_output));
    output->wlr_output        = wlr_output;
    output->server            = server;

    output->frame.notify = handle_frame_notify;
    wl_signal_add(&wlr_output->events.frame, &output->frame);
    output->request_state.notify = handle_request_state;
    wl_signal_add(&wlr_output->events.request_state, &output->request_state);

    // temp background color:
    // {0.730473, 0.554736, 0.665036, 1.000000} is really pretty.
    output->color[0] = float_rand();
    output->color[1] = float_rand();
    output->color[2] = float_rand();
    output->color[3] = 1.0;

    printf("Output color: {%f, %f, %f, %f}\n", output->color[0],
           output->color[1], output->color[2], output->color[3]);

    return output;
}

static void handle_new_output(struct wl_listener *listener, void *data) {
    wlr_log(WLR_DEBUG, "New output detected");
    struct hrt_server *server = wl_container_of(listener, server, new_output);

    struct wlr_output *wlr_output = data;

    wlr_output_init_render(wlr_output, server->allocator, server->renderer);

    struct wlr_output_state state;
    wlr_output_state_init(&state);
    wlr_output_state_set_enabled(&state, true);

    struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
    if (mode != NULL) {
        wlr_output_state_set_mode(&state, mode);
    }

    if (!wlr_output_commit_state(wlr_output, &state)) {
        // FIXME: Actually do some error handling instead of just logging:
        wlr_log(WLR_ERROR, "Output state could not be commited");
    }
    wlr_output_state_finish(&state);

    struct wlr_output_layout_output *l_output =
        wlr_output_layout_add_auto(server->output_layout, wlr_output);
    struct wlr_scene_output *scene_output =
        wlr_scene_output_create(server->scene, wlr_output);
    wlr_scene_output_layout_add_output(server->scene_layout, l_output,
                                       scene_output);

    struct hrt_output *output = hrt_output_create(server, wlr_output);

    output->destroy.notify = handle_output_destroy;
    wl_signal_add(&wlr_output->events.destroy, &output->destroy);

    server->output_callback->output_added(output);
}

static void handle_output_manager_destroy(struct wl_listener *listener,
                                          void *data) {
    wlr_log(WLR_DEBUG, "Output Manager destroyed");

    struct hrt_server *server =
        wl_container_of(listener, server, output_manager_destroy);

    wl_list_remove(&server->output_manager_apply.link);
    wl_list_remove(&server->output_manager_test.link);
    wl_list_remove(&server->output_manager_destroy.link);
}

static void handle_output_manager_apply(struct wl_listener *listener,
                                        void *data) {}

static void handle_output_manager_test(struct wl_listener *listener,
                                       void *data) {}

static void handle_output_layout_changed(struct wl_listener *listener,
                                         void *data) {
    struct hrt_server *server =
        wl_container_of(listener, server, output_layout_changed);
    // struct wlr_output_layout *output_layout = data;

    server->output_callback->output_layout_changed();
}

bool hrt_output_init(struct hrt_server *server,
                     const struct hrt_output_callbacks *callbacks) {
    server->output_callback   = callbacks;
    server->new_output.notify = handle_new_output;
    wl_signal_add(&server->backend->events.new_output, &server->new_output);

    server->output_layout = wlr_output_layout_create(server->wl_display);
    server->scene         = wlr_scene_create();
    server->scene_layout =
        wlr_scene_attach_output_layout(server->scene, server->output_layout);

    server->output_layout_changed.notify = handle_output_layout_changed;
    wl_signal_add(&server->output_layout->events.change,
                  &server->output_layout_changed);

    server->output_manager = wlr_output_manager_v1_create(server->wl_display);

    if (!server->output_manager) {
        return false;
    }
    server->output_manager_apply.notify = handle_output_manager_apply;
    wl_signal_add(&server->output_manager->events.apply,
                  &server->output_manager_apply);
    server->output_manager_test.notify = handle_output_manager_test;
    wl_signal_add(&server->output_manager->events.apply,
                  &server->output_manager_test);
    server->output_manager_destroy.notify = handle_output_manager_destroy;
    wl_signal_add(&server->output_manager->events.destroy,
                  &server->output_manager_destroy);

    // temporary random seed:
    srand(time(0));

    return true;
}

void hrt_output_destroy(struct hrt_server *server) {
    wlr_scene_node_destroy(&server->scene->tree.node);
    wl_list_remove(&server->output_layout_changed.link);
    // The output layout  gets destroyed when the display does:
    // wlr_output_layout_destroy(server->output_layout);
}
