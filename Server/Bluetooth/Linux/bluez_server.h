#ifndef BLUEZ_SERVER_H
#define BLUEZ_SERVER_H

#include <bluetooth/sdp_lib.h>

typedef struct {
    int client;
    int serverSock;
    sdp_session_t *session;
} server_info_t;

server_info_t *init_server();
char *read_server(const server_info_t * const info);
void write_server(const server_info_t * const info, const char * const message);
void close_server(server_info_t *info);

#endif