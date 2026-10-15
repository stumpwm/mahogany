#ifndef HRT_SESSION_LOCK_IMPL_H
#define HRT_SESSION_LOCK_IMPL_H

#include "hrt/hrt_server.h"

bool session_lock_manager_init(struct hrt_server *server);

void session_lock_arrange(struct hrt_server *server);

void session_lock_output_arrange(struct hrt_server *server,
                                 struct hrt_output *output);

#endif
