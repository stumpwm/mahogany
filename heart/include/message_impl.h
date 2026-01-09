#pragma once

#include <stdbool.h>

#include "hrt/hrt_server.h"

bool hrt_message_init(struct hrt_server *server);
void hrt_message_destroy(struct hrt_server *server);
