#ifndef HRT_HRT_SESSION_LOCK__H
#define HRT_HRT_SESSION_LOCK__H

#include "hrt/hrt_server.h"
#include <stdbool.h>

struct hrt_server;

struct hrt_session_lock_callbacks {
    void (*session_locked)();
    void (*session_unlocked)();
};

bool hrt_session_lock_manager_init(struct hrt_server *server,
                               struct hrt_session_lock_callbacks *callbacks);

bool hrt_session_locked(struct hrt_server *server);

#endif
