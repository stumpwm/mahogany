#include <stdio.h>

#include <wlr/util/log.h>
#include <hrt_server.h>

int main(int argc, char *argv[]) {
  wlr_log_init(WLR_DEBUG, NULL);

  struct hrt_server server;

  if(!hrt_server_init(&server)) {
    return 1;
  }

  hrt_server_start(&server);
  hrt_server_run(&server);
  hrt_server_finish(&server);
  return 0;
}
