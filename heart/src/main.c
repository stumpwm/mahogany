#include <stdio.h>

#include <hrt_server.h>

int main(int argc, char *argv[]) {
  puts("Hello, World!");

  struct hrt_server server;

  if(!hrt_server_init(&server)) {
    return 1;
  }

  hrt_server_start(&server);
  hrt_server_run(&server);
  hrt_server_finish(&server);
  return 0;
}
