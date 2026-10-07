#ifndef IDLE_IMPL_H
#define IDLE_IMPL_H

#include <stdbool.h>

struct hrt_seat;
struct hrt_server;

bool hrt_idle_init(struct hrt_server *server);

/**
 * Report user activity on a seat, restarting the timeouts clients are
 * waiting on.
 *
 * This must be called from every input handler that counts as user activity.
 */
void hrt_idle_notify_activity(struct hrt_seat *seat);

/**
 * Checks if any surfaces on the screen are asking for inhibit,
 * updating the idle notifier inhibited state.
 *
 * It must be called by code that changes what is on the screen
 * and by protocol handlers.
 *
 * We defer to an idle callback so it catches a scene graph
 * after every listener of the current event has run.
 */
void hrt_idle_inhibit_schedule(struct hrt_server *server);

#endif
