#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#include <wlr/render/wlr_renderer.h>

#include <hrt/hrt_output.h>

static void handle_frame_notify(struct wl_listener *listener, void *data) {
  struct hrt_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->server->scene;

  struct wlr_scene_output *scene_output = wlr_scene_get_scene_output(scene, output->wlr_output);
  wlr_scene_output_commit(scene_output);

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(scene_output, &now);
}

static void handle_output_destroy(struct wl_listener *listener, void *data) {
  wlr_log(WLR_DEBUG, "Output destroyed");
  struct hrt_output *output = wl_container_of(listener, output, destroy);
  struct hrt_server *server = output->server;
  server->output_callback->output_removed(output);

  // wlr_output_layout removes the output by itself.

  free(output);
}

// temp random float generator
static float float_rand()
{
  return (float) (rand() / (double) RAND_MAX); /* [0, 1.0] */
}

static struct hrt_output *hrt_output_create(struct hrt_server *server,
				     struct wlr_output *wlr_output) {
  struct hrt_output *output = calloc(1, sizeof(struct hrt_output));
  output->wlr_output = wlr_output;
  output->server = server;

  wlr_output_init_render(wlr_output, server->allocator, server->renderer);

  output->frame.notify = handle_frame_notify;
  wl_signal_add(&wlr_output->events.frame, &output->frame);

  // temp background color:
  // {0.730473, 0.554736, 0.665036, 1.000000} is really pretty.
  output->color[0] = float_rand();
  output->color[1] = float_rand();
  output->color[2] = float_rand();
  output->color[3] = 1.0;

  printf("Output color: {%f, %f, %f, %f}\n", output->color[0], output->color[1], output->color[2],
	 output->color[3]);

  return output;
}

static void handle_new_output(struct wl_listener *listener, void *data) {
  wlr_log(WLR_DEBUG, "New output detected");
  struct hrt_server *server = wl_container_of(listener, server, new_output);

  struct wlr_output *wlr_output = data;

  struct hrt_output *output = hrt_output_create(server, wlr_output);

  output->destroy.notify = handle_output_destroy;
  wl_signal_add(&wlr_output->events.destroy, &output->destroy);

  struct wlr_output_mode *mode = wlr_output_preferred_mode(wlr_output);
  if (mode != NULL) {
    wlr_output_set_mode(wlr_output, mode);
    wlr_output_enable(wlr_output, true);
    wlr_output_commit(wlr_output);
  }

  wlr_output_layout_add_auto(server->output_layout, wlr_output);

  server->output_callback->output_added(output);
}

static void handle_output_manager_apply(struct wl_listener *listener, void *data) {

}

static void handle_output_manager_test(struct wl_listener *listener, void *data) {

}

bool hrt_output_init(struct hrt_server *server, const struct hrt_output_callbacks *callbacks) {
  server->output_callback = callbacks;
  server->new_output.notify = handle_new_output;
  wl_signal_add(&server->backend->events.new_output, &server->new_output);

  server->output_layout = wlr_output_layout_create();
  server->scene = wlr_scene_create();
  wlr_scene_attach_output_layout(server->scene, server->output_layout);

  server->output_manager = wlr_output_manager_v1_create(server->wl_display);

  if(!server->output_manager) {
    return false;
  }
  server->output_manager_apply.notify = handle_output_manager_apply;
  server->output_manager_test.notify = handle_output_manager_test;

  // temporary random seed:
  srand(time(0));

  return true;
}
