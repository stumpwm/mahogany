#pragma once

#include "hrt/hrt_server.h"

bool hrt_output_init(struct hrt_server *server,
		     const struct hrt_output_callbacks *callbacks);
void hrt_output_destroy(struct hrt_server *server);
