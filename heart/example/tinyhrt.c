#include <stdlib.h>
#include <stdio.h>

#include <wlr/util/log.h>
#include <wlr/types/wlr_cursor.h>

#include <hrt/hrt_server.h>
#include <hrt/hrt_output.h>
#include <hrt/hrt_input.h>
#include <hrt/hrt_view.h>
#include <hrt/hrt_message.h>
#include <hrt/hrt_scene.h>

struct example_output {
    struct hrt_output *output;
    struct wl_list link;
};

struct example_server {
    struct hrt_server server;
    struct hrt_scene_group *group;
    struct wl_list outputs;
    struct example_output *current_output;
    int message_counter;
    enum window_gravity message_gravity;
};

static struct example_server server = {0};

static struct wl_list *next_output() {
    struct wl_list *next = server.current_output->link.next;
    if (next == &server.outputs)
        next = server.outputs.next;
    return next;
}

/**
 * In mahogany, we keep a separate data structure that contains each
 * toplevel's position; what we are doing here uses the implementation details of
 * hrt's design; if you were writing a compositor using hrt, you'd want to
 * use your own datastructure or work on help formalize and document how
 * this works
 **/
static struct wlr_scene_tree *find_toplevel_at(struct hrt_server *server, double lx, double ly,
                          struct wlr_surface **surface, double *sx,
                          double *sy) {
    /* This returns the topmost node in the scene at the given layout coords.
     * We only care about surface nodes as we are specifically looking for a
     * surface in the surface tree of a tinywl_toplevel. */
    struct wlr_scene_node *node =
        wlr_scene_node_at(&server->scene->tree.node, lx, ly, sx, sy);
    if (node == NULL || node->type != WLR_SCENE_NODE_BUFFER) {
        return NULL;
    }
    struct wlr_scene_buffer *scene_buffer = wlr_scene_buffer_from_node(node);
    struct wlr_scene_surface *scene_surface =
        wlr_scene_surface_try_from_buffer(scene_buffer);
    if (!scene_surface) {
        return NULL;
    }

    *surface = scene_surface->surface;
    /* Find the node corresponding to the toplevel at the root of this
     * surface tree, it is the only one for which we set the data field. */
    struct wlr_scene_tree *tree = node->parent;
    while (tree != NULL && tree->node.data == NULL) {
        tree = tree->node.parent;
    }
    if (tree) {
        return tree;
    } else {
        return NULL;
    }
}

static void cursor_button_callback(struct hrt_seat *seat,
                                   struct wlr_pointer_button_event *event) {
    double sx, sy;
    struct wlr_surface *found_surface = NULL;
    struct wlr_scene_tree *toplevel =
        find_toplevel_at(&server.server, seat->cursor->x, seat->cursor->y,
                         &found_surface, &sx, &sy);

    if (toplevel) {
        struct hrt_view *view = toplevel->node.data;
        hrt_view_focus(view, seat);
    }

    hrt_seat_notify_button(seat, event);
    puts("Cursor callback called");
}

static void cursor_wheel_callback(struct hrt_seat *seat,
                                  struct wlr_pointer_axis_event *event) {
    puts("Cursor callback called");
    hrt_seat_notify_axis(seat, event);
}

static void output_added_callback(struct hrt_output *output) {
    printf("Output added callback called, scale=%f\n",
           output->wlr_output->scale);
    hrt_output_init(output, nullptr);

    struct example_output *o = calloc(1, sizeof(*o));
    o->output                = output;
    wl_list_insert(&server.outputs, &o->link);
    server.current_output = o;
}

static void output_removed_callback(struct hrt_output *output) {
    puts("Output removed callback called");
    struct example_output *o = NULL, *tmp;

    wl_list_for_each_safe(o, tmp, &server.outputs, link) {
        if (o->output == output) {
            if (o == server.current_output) {
                server.current_output = (wl_list_length(&server.outputs) > 1) ?
                    wl_container_of(next_output(), server.current_output,
                                    link) :
                    NULL;
            }
            wl_list_remove(&o->link);
            free(o);
            break;
        }
    }
}

static void new_view_callback(struct hrt_view *view) {
    puts("New view callback called!");
    hrt_view_send_configure(view);
}

static void view_mapped(struct hrt_view *view) {
    puts("View Mapped");
    hrt_scene_layer_add_view(server.group->layers, view);
}

static void view_unmapped(struct hrt_view *view) {
    puts("View unmapped");
    // We could put the view back to where it defaults to in the frame
    // tree, but that's unneeded; it won't be displayed anyways.
}

static void view_size_changed(struct hrt_view *view) {
    puts("View size changed");
}

static void view_callback(struct hrt_view *view) {
    puts("Generic callback called");
}

static bool view_fullscreen_callback(struct hrt_view *view,
                                     struct hrt_output *output, bool set) {
    puts("Generic callback called");
    return false;
}

static void view_destroy_callback(struct hrt_view *view) {
    puts("View destroy callback called");
}

static bool keyboard_callback(struct hrt_seat *seat,
                              struct hrt_keypress_info *info) {
    puts("Keyboard callback called");
    printf("Modifiers: %d\n", info->modifiers);
    printf("Keys pressed:");
    for (size_t i = 0; i < info->keysyms_len; ++i) {
        if (info->keysyms[i] == XKB_KEY_Escape) {
            puts("Exiting due to escape pressed");
            hrt_server_stop(seat->server);
        }
        char buffer[20];
        xkb_keysym_get_name(info->keysyms[i], buffer, sizeof(buffer));
        printf(" %s", buffer);
        if (strcmp(buffer, "o") == 0 && wl_list_length(&server.outputs) > 1) {
            server.current_output =
                wl_container_of(next_output(), server.current_output, link);
            printf("selected output %s\n",
                   server.current_output->output->wlr_output->name);
        }
    }
    puts("\n\n");
    return false;
}

static void layout_changed() {}

static const struct hrt_output_callbacks output_callbacks = {
    .output_added          = &output_added_callback,
    .output_removed        = &output_removed_callback,
    .output_layout_changed = &layout_changed,
};

static const struct hrt_seat_callbacks seat_callbacks = {
    .button_event            = &cursor_button_callback,
    .wheel_event             = &cursor_wheel_callback,
    .keyboard_keypress_event = &keyboard_callback,
};

static const struct hrt_view_callbacks view_callbacks = {
    .new_view           = &new_view_callback,
    .view_destroyed     = &view_destroy_callback,
    .view_size_changed  = &view_size_changed,
    .view_mapped        = &view_mapped,
    .view_unmapped      = &view_unmapped,
    .request_minimize   = &view_callback,
    .request_maximize   = &view_callback,
    .request_fullscreen = &view_fullscreen_callback,
};

int main(int argc, char *argv[]) {
    wlr_log_init(WLR_DEBUG, NULL);

    wl_list_init(&server.outputs);

    if (!hrt_server_init(&server.server, &output_callbacks, &seat_callbacks,
                         &view_callbacks, NULL, WLR_DEBUG)) {
        return 1;
    }

    server.group = hrt_server_group_create(&server.server);

    hrt_server_start(&server.server);
    hrt_server_finish(&server.server);
    return 0;
}
