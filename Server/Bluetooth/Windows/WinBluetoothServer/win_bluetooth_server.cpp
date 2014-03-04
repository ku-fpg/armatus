#include <WinSock2.h>
#include <ws2bth.h>
#include <bthsdpdef.h>
#include <BluetoothAPIs.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tchar.h>
#include "win_bluetooth_server.h"

#pragma comment(lib, "Ws2_32.lib")
#pragma comment(lib, "irprops.lib")

/**
 * Adapted from http://www.codeproject.com/Articles/252882/Bluetooth-Server-Programming-on-Windows,
 * under The Code Project Open License (CPOL).
 * 
 * Copyright (c) 2011 Vijay Rajanna
 */

char *GetLastErrorMessage(DWORD last_error)
{
	static char errmsg[512];

	if (!FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,
		0, last_error, 0, errmsg, 511, NULL)) {
		/* if we fail, call ourself to find out why and return that error */
		sprintf_s(errmsg, "Format message failed with 0x%x\n", GetLastError());
	}
	return errmsg;
}

server_info_t *init_server() {
	WORD wVersionRequested = 0x202;
	WSADATA m_data;
	if (0 == WSAStartup(wVersionRequested, &m_data)) {
		SOCKET s = socket(AF_BTH, SOCK_STREAM, BTHPROTO_RFCOMM);
		const DWORD lastError = ::GetLastError();

		if (s == INVALID_SOCKET) {
			printf("Failed to get bluetooth socket! %s\n",
				GetLastErrorMessage(lastError));
			exit(1);
		}
		WSAPROTOCOL_INFO protocolInfo;
		int protocolInfoSize = sizeof(protocolInfo);

		if (0 != getsockopt(s, SOL_SOCKET, SO_PROTOCOL_INFO,
			(char*)&protocolInfo, &protocolInfoSize)) {
			exit(1);
		}
		SOCKADDR_BTH address;
		address.addressFamily = AF_BTH;
		address.btAddr = 0;
		address.serviceClassId = GUID_NULL;
		address.port = BT_PORT_ANY;
		sockaddr *pAddr = (sockaddr*)&address;

		if (0 != bind(s, pAddr, sizeof(SOCKADDR_BTH))) {
			printf("%s\n", GetLastErrorMessage(GetLastError()));
		} else {
			printf("\nBinding Successful....\n");
			int length = sizeof(SOCKADDR_BTH);
			getsockname(s, (sockaddr*)&address, &length);
			wprintf(L"Local Bluetooth device is %04x%08x \nServer channel = %d\n",
				GET_NAP(address.btAddr), GET_SAP(address.btAddr), address.port);
		}

		int size = sizeof(SOCKADDR_BTH);
		if (0 != getsockname(s, pAddr, &size)) {
			printf("%s\n", GetLastErrorMessage(GetLastError()));
		}
		if (0 != listen(s, 10)) {
			printf("%s\n", GetLastErrorMessage(GetLastError()));
		}

		WSAQUERYSET *service = new WSAQUERYSET;
		memset(service, 0, sizeof(*service));
		service->dwSize = sizeof(*service);
		service->lpszServiceInstanceName = _T("Accelerometer Data...");
		service->lpszComment = _T("Pushing data to PC");

		GUID serviceID = SerialPortServiceClass_UUID; // OBEXFileTransferServiceClass_UUID;

		service->lpServiceClassId = &serviceID;
		service->dwNumberOfCsAddrs = 1;
		service->dwNameSpace = NS_BTH;

		CSADDR_INFO csAddr;
		memset(&csAddr, 0, sizeof(csAddr));
		csAddr.LocalAddr.iSockaddrLength = sizeof(SOCKADDR_BTH);
		csAddr.LocalAddr.lpSockaddr = pAddr;
		csAddr.iSocketType = SOCK_STREAM;
		csAddr.iProtocol = BTHPROTO_RFCOMM;
		service->lpcsaBuffer = &csAddr;

		if (0 != WSASetService(service, RNRSERVICE_REGISTER, 0)) {
			printf("Service registration failed....");
			printf("%d\n", GetLastErrorMessage(GetLastError()));
		} else {
			printf("\nService registration Successful....\n");
		}
		printf("\nBefore accept.........");
		SOCKADDR_BTH sab2;
		int ilen = sizeof(sab2);
		SOCKET s2 = accept(s, (sockaddr*)&sab2, &ilen);
		if (s2 == INVALID_SOCKET) {
			wprintf(L"Socket bind, error %d\n", WSAGetLastError());
		}
		wprintf(L"\nConnection came from %04x%08x to channel %d\n",
			GET_NAP(sab2.btAddr), GET_SAP(sab2.btAddr), sab2.port);
		wprintf(L"\nAfter Accept\n");

		server_info_t *info = new server_info_t;
		info->client = s2;
		info->serverSock = s;
		info->session = service;

		return info;
	} else {
		printf("Unable to start WSA. %s\n", GetLastErrorMessage(GetLastError()));
		exit(1);
	}
}

char *read_server(const server_info_t * const info) {
	char buffer[1024] = { 0 };
	// Use malloc so that Haskell can use free. It's easier this way. No, really.
	char *inputCpy = (char *)malloc(sizeof(char)* (1024 + 1));
	memset(buffer, 0, sizeof(buffer));
	int bytes_read = recv(info->client, (char*)buffer, sizeof(buffer), 0);
	strcpy_s(inputCpy, sizeof(inputCpy), buffer);
	if (bytes_read > 0) {
		printf("received [%s]\n", inputCpy);
	}

	return inputCpy;
}

void write_server(const server_info_t * const info, const char * const message) {
	char messageCpy[1024] = { 0 };
	strcpy_s(messageCpy, message);
	int bytes_sent = send(info->client, (char*)messageCpy, sizeof(messageCpy), 0);
	if (bytes_sent > 0) {
		printf("sent [%s]\n", messageCpy);
	}
}

void close_server(server_info_t * info) {
	closesocket(info->client);
	if (0 != WSASetService(info->session, RNRSERVICE_DELETE, 0)) {
		printf("%s\n", GetLastErrorMessage(GetLastError()));
	}
	closesocket(info->serverSock);
	WSACleanup();
	delete info->session;
	delete info;
}

//int _tmain(int argc, _TCHAR* argv[])
//{
//	WORD wVersionRequested = 0x202;
//	WSADATA m_data;
//	if (0 == WSAStartup(wVersionRequested, &m_data))
//	{
//		SOCKET s = socket(AF_BTH, SOCK_STREAM, BTHPROTO_RFCOMM);
//		const DWORD lastError = ::GetLastError();
//
//		if (s == INVALID_SOCKET)
//		{
//			printf("Failed to get bluetooth socket! %s\n",
//				GetLastErrorMessage(lastError));
//			exit(1);
//		}
//		WSAPROTOCOL_INFO protocolInfo;
//		int protocolInfoSize = sizeof(protocolInfo);
//
//		if (0 != getsockopt(s, SOL_SOCKET, SO_PROTOCOL_INFO,
//			(char*)&protocolInfo, &protocolInfoSize))
//		{
//			exit(1);
//		}
//		SOCKADDR_BTH address;
//		address.addressFamily = AF_BTH;
//		address.btAddr = 0;
//		address.serviceClassId = GUID_NULL;
//		address.port = BT_PORT_ANY;
//		sockaddr *pAddr = (sockaddr*)&address;
//
//		if (0 != bind(s, pAddr, sizeof(SOCKADDR_BTH)))
//		{
//			printf("%s\n", GetLastErrorMessage(GetLastError()));
//		}
//		else
//		{
//			printf("\nBinding Successful....\n");
//			int length = sizeof(SOCKADDR_BTH);
//			getsockname(s, (sockaddr*)&address, &length);
//			wprintf(L"Local Bluetooth device is %04x%08x \nServer channel = %d\n",
//				GET_NAP(address.btAddr), GET_SAP(address.btAddr), address.port);
//		}
//
//		int size = sizeof(SOCKADDR_BTH);
//		if (0 != getsockname(s, pAddr, &size))
//		{
//			printf("%s\n", GetLastErrorMessage(GetLastError()));
//		}
//		if (0 != listen(s, 10))
//		{
//			printf("%s\n", GetLastErrorMessage(GetLastError()));
//		}
//
//		WSAQUERYSET *service = new WSAQUERYSET;
//		memset(service, 0, sizeof(*service));
//		service->dwSize = sizeof(*service);
//		service->lpszServiceInstanceName = _T("Accelerometer Data...");
//		service->lpszComment = _T("Pushing data to PC");
//
//		GUID serviceID = SerialPortServiceClass_UUID; // OBEXFileTransferServiceClass_UUID;
//
//		service->lpServiceClassId = &serviceID;
//		service->dwNumberOfCsAddrs = 1;
//		service->dwNameSpace = NS_BTH;
//
//		CSADDR_INFO csAddr;
//		memset(&csAddr, 0, sizeof(csAddr));
//		csAddr.LocalAddr.iSockaddrLength = sizeof(SOCKADDR_BTH);
//		csAddr.LocalAddr.lpSockaddr = pAddr;
//		csAddr.iSocketType = SOCK_STREAM;
//		csAddr.iProtocol = BTHPROTO_RFCOMM;
//		service->lpcsaBuffer = &csAddr;
//
//		if (0 != WSASetService(service, RNRSERVICE_REGISTER, 0))
//		{
//			printf("Service registration failed....");
//			printf("%d\n", GetLastErrorMessage(GetLastError()));
//		}
//		else
//		{
//			printf("\nService registration Successful....\n");
//		}
//		printf("\nBefore accept.........");
//		SOCKADDR_BTH sab2;
//		int ilen = sizeof(sab2);
//		SOCKET s2 = accept(s, (sockaddr*)&sab2, &ilen);
//		if (s2 == INVALID_SOCKET)
//		{
//			wprintf(L"Socket bind, error %d\n", WSAGetLastError());
//		}
//		wprintf(L"\nConnection came from %04x%08x to channel %d\n",
//			GET_NAP(sab2.btAddr), GET_SAP(sab2.btAddr), sab2.port);
//		wprintf(L"\nAfter Accept\n");
//
//		char buffer[1024] = { 0 };
//		memset(buffer, 0, sizeof(buffer));
//		recv(s2, (char*)buffer, sizeof(buffer), 0);
//		printf("%s\n", buffer);
//
//		char response[] = "Say Hi to your mother for me.";
//		printf("%s\n", response);
//		send(s2, (char*)response, sizeof(response), 0);
//
//		closesocket(s2);
//		if (0 != WSASetService(service, RNRSERVICE_DELETE, 0))
//		{
//			printf("%s\n", GetLastErrorMessage(GetLastError()));
//		}
//		closesocket(s);
//		WSACleanup();
//		delete service;
//	}
//}