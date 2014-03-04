#ifndef WIN_BLUETOOTH_SERVER_H
#define WIN_BLUETOOTH_SERVER_H

#include <WinSock2.h>

typedef struct {
	SOCKET client;
	SOCKET serverSock;
	WSAQUERYSET *session;
} server_info_t;

server_info_t *init_server();
char *read_server(const server_info_t * const info);
void write_server(const server_info_t * const info, const char * const message);
void close_server(const server_info_t *info);

#endif