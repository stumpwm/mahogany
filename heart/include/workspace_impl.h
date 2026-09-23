#ifndef HRT_WORKSPACE
#define HRT_WORKSPACE

#include "hrt/hrt_server.h"
#include "hrt/hrt_workspace.h"

bool hrt_workspace_init(struct hrt_server *sever,
                        struct hrt_workspace_callbacks *callbacks);

#endif
