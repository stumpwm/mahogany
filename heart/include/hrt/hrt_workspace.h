#ifndef HRT_HRT_WORKSPACE_H
#define HRT_HRT_WORKSPACE_H

#include <hrt/hrt_server.h>

// Callbacks for requested actions on workspaces;
// if the entry is null, then that feature is not
// advertised to clients
struct hrt_workspace_callbacks {
    void (*create_workspace)(char *name, void *group);
    void (*activate_workspace)(void *workspace);
    void (*decativate_workspace)(void *workspace);
    void (*assign_workspace)(void *workspace, void *group);
    void (*remove_workspace)(void *workspace);
};

bool hrt_workspace_init(struct hrt_server *sever,
                        struct hrt_workspace_callbacks *callbacks);

typedef struct wlr_ext_workspace_group_handle_v1 hrt_workspace_group_handle;
typedef struct wlr_ext_workspace_handle_v1 hrt_workspace_handle;

hrt_workspace_group_handle *
hrt_workspace_group_handle_create(struct hrt_server *server);

void hrt_workspace_group_handle_v1_destroy(hrt_workspace_group_handle *group);

void hrt_workspace_group_add_output(hrt_workspace_group_handle *group,
                                    struct hrt_output *output);
void hrt_workspace_group_handle_output_leave(hrt_workspace_group_handle *group,
                                             struct hrt_output *output);

hrt_workspace_handle *hrt_workspace_handle_create(struct hrt_server *server,
                                                  const char *id);

void hrt_workspace_handle_destroy(hrt_workspace_handle *workspace);

void hrt_workspace_handle_set_group(hrt_workspace_handle *workspace,
                                    hrt_workspace_group_handle *group);
void hrt_workspace_handle_set_name(hrt_workspace_handle *workspace,
                                   const char *name);
void hrt_workspace_handle_set_coordinates(hrt_workspace_handle *workspace,
                                          const uint32_t *coords,
                                          size_t coords_len);
void hrt_workspace_handle_set_active(hrt_workspace_handle *workspace,
                                     bool enabled);
void hrt_workspace_handle_set_urgent(hrt_workspace_handle *workspace,
                                     bool enabled);
void hrt_workspace_handle_set_hidden(hrt_workspace_handle *workspace,
                                     bool enabled);

#endif
