#include <stdio.h>

#include <mahogany_server.h>

int main(int argc, char *argv[]) {
  puts("Hello, World!");

  struct mahogany_server server;

  if(!mahogany_server_init(&server)) {
    return 1;
  }

  mahogany_server_start(&server);
  mahogany_server_run(&server);
  mahogany_server_finish(&server);
  return 0;
}
