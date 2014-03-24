{-|
    
This file describes Haskell's interface to win_bluetooth_server.h, from which it calls the functions that deal with the Windows Bluetooth libraries. This uses some markup from the hsc2hs utility to make it easier to marshal and unmarshal Haskell data types into structs. To build a .hs file from this, run the following command in a terminal:

    hsc2hs WinBluetoothServer.chs

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module BlueZServer where
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Win32.Types

#include <guiddef.h> -- This may not be needed, include it for good measure
#include <ws2def.h>
#include <WTypesbase.h>
#include <WinSock2.h>
#include "win_bluetooth_server.h"

data GUID = GUID {
	data1 :: CULong,
	data2 :: CUShort,
	data3 :: CUShort,
	data4 :: Ptr CUChar -- unsigned char Data4[8]
}

instance Storable GUID where
	sizeOf = (#size GUID)
	alignment _ = alignment (undefined :: CULong)
	peek p = do
		data1' <- (#peek GUID, Data1) p
		data2' <- (#peek GUID, Data2) p
		data3' <- (#peek GUID, Data3) p
		data4' <- (#peek GUID, Data4) p
		return GUID $ data1' data2' data3' data4'
	poke p (GUID data1' data2' data3' data4') = do
		(#poke GUID, Data1) p data1'
		(#poke GUID, Data2) p data2'
		(#poke GUID, Data3) p data3'
		(#poke GUID, Data4) p data4'

#{enum WSAECOMPARATOR, WSAECOMPARATOR
	, COMP_EQUAL = 0,
	, COMP_NOTLESS
	}

data WSAVERSION = WSAVERSION {
	dwVersion :: DWORD,
	ecHow :: WSAECOMPARATOR
}

instance Storable WSAVERSION where
	sizeOf _ = (#size WSAVERSION)
	alignment _ = alignment (undefined :: DWORD)
	peek p = do
		dwVersion' <- (#peek WSAVERSION, dwVersion) p
		ecHow' <- (#peek WSAVERSION, ecHow) p
		return WSAVERSION $ dwVersion' ecHow'
	poke p (WSAVERSION dwVersion' ecHow') = do
		(#poke WSAVERSION, dwVersion) p dwVersion'
		(#poke WSAVERSION, ecHow) p ecHow'

data AFPROTOCOLS = AFPROTOCOLS {
	iAddressFamily :: CInt,
	iProtocol :: CInt
}

instance Storable AFPROTOCOLS where
	sizeOf _ = (#size AFPROTOCOLS)
	alignment _ = alignment (undefined :: CInt)
	peek p = do
		iAddressFamily' <- (#peek AFPROTOCOLS, iAddressFamily) p
		iProtocol' <- (#peek AFPROTOCOLS, iProtocol) p
		return AFPROTOCOLS $ iAddressFamily' iProtocol'
	poke p (AFPROTOCOLS iAddressFamily' iProtocol') = do
		(#poke AFPROTOCOLS, iAddressFamily) p iAddressFamily'
		(#poke AFPROTOCOLS, iProtocol) p iProtocol'

type ADDRESS_FAMILY = USHORT

data SOCKADDR = SOCKADDR {
	saFamily :: ADDRESS_FAMILY,
	saData :: Ptr CChar -- CHAR sa_data[14];
}

instance Storable SOCKADDR where
	sizeOf _ = (#size SOCKADDR)
	alignment _ = alignment (undefined :: ADDRESS_FAMILY)
	peek p = do
		saFamily' <- (#peek SOCKADDR, sa_family) p
		saData' <- (#peek SOCKADDR, sa_data) p
		return SOCKADDR $ saFamily' saData'
	poke p (SOCKADDR saFamily' saData') = do
		(#poke SOCKADDR, sa_family) p saFamily'
		(#poke SOCKADDR, sa_data) p saData'

type LPSOCKADDR = Ptr SOCKADDR

data SOCKET_ADDRESS = SOCKET_ADDRESS {
	lpSockaddr :: LPSOCKADDR,
	iSockaddrLength :: CInt
}

instance Storable SOCKET_ADDRESS where
	sizeOf _ = (#size SOCKET_ADDRESS)
	alignment _ = alignment (undefined :: CInt)
	peek p = do
		lpSockaddr' <- (#peek SOCKET_ADDRESS, lpSockaddr) p
		iSockaddrLength' <- (#peek SOCKET_ADDRESS, iSockaddrLength) p
		return SOCKET_ADDRESS $ lpSockaddr' iSockaddrLength'
	poke p (SOCKET_ADDRESS lpSockaddr' iSockaddrLength') = do
		(#poke SOCKET_ADDRESS, lpSockaddr) p lpSockaddr'
		(#poke SOCKET_ADDRESS, iSockaddrLength) p iSockaddrLength

data CSADDR_INFO = CSADDR_INFO {
	localAddr :: SOCKET_ADDRESS,
	remoteAddr :: SOCKET_ADDRESS,
	iSocketType :: CInt,
	iProtocolCsAddr :: CInt -- INT iProtocol
}

instance Storable CSADDR_INFO where
	sizeOf _ = (#size CSADDR_INFO)
	alignment _ = alignment (undefined :: SOCKET_ADDRESS)
	peek p = do
		localAddr' <- (#peek CSADDR_INFO, LocalAddr) p
		remoteAddr' <- (#peek CSADDR_INFO, RemoteAddr) p
		iSocketType' <- (#peek CSADDR_INFO, iSocketType) p
		iProtocolCsAddr' <- (#peek CSADDR_INFO, iProtocol) p
		return CSADDR_INFO $ localAddr' remoteAddr' iSocketType' iProtocolCsAddr'
	poke p (CSADDR_INFO localAddr' remoteAddr' iSocketType' iProtocolCsAddr') = do
		(#poke CSADDR_INFO, LocalAddr) p localAddr'
		(#poke CSADDR_INFO, RemoteAddr) p remoteAddr'
		(#poke CSADDR_INFO, iSocketType) p iSocketType'
		(#poke CSADDR_INFO, iProtocol) p iProtocolCsAddr'

data BLOB = BLOB {
	cbSize :: CULong,
	pBlobData :: Ptr CUChar
}

instance Storable BLOB where
	sizeOf _ = (#size BLOB)
	alignment _ = alignment (undefined :: CULong)
	peek p = do
		cbSize' <- (#peek BLOB, cbSize) p
		pBlobData' <- (#peek BLOB, pBlobData) p
		return BLOB $ cbSize' pBlobData'
	poke p (BLOB cbSize' pBlobData') = do
		(#poke BLOB, cbSize) p cbSize'
		(#poke BLOB, pBlobData) p pBlobData'

type LPGUID = Ptr GUID
type LPWSAVERSION = Ptr WSAVERSION
type LPAFPROTOCOLS = Ptr AFPROTOCOLS
type LPCSADDR_INFO = Ptr CSADDR_INFO
type LPBLOB = Ptr BLOB

data WSAQuerySet = WSAQuerySet {
	dwSize :: DWORD,
    lpszServiceInstanceName :: LPWSTR,
	lpServiceClassId :: LPGUID,
	lpVersion :: LPWSAVERSION,
	lpszComment :: LPWSTR,
	dwNameSpace :: DWORD,
	lpNSProviderId :: LPGUID,
	lpszContext :: LPWSTR,
	dwNumberOfProtocols :: DWORD,
	lpafpProtocols :: LPAFPROTOCOLS,
	lpszQueryString :: LPWSTR,
	dwNumberOfCsAddrs :: DWORD,
	lpcsaBuffer :: LPCSADDR_INFO,
	dwOutputFlags :: DWORD,
	lpBlob :: LPBLOB
}

instance Storable WSAQuerySet where
    sizeOf _ = (#size WSAQUERYSETW)
    alignment _ = alignment (undefined :: DWORD)
    peek p = do
        dwSize' <- (#peek WSAQUERYSETW, dwSize) p
        lpszServiceInstanceName' <- (#peek WSAQUERYSETW, lpszServiceInstanceName) p
        lpServiceClassId' <- (#peek WSAQUERYSETW, lpServiceClassId) p
        lpVersion' <- (#peek WSAQUERYSETW, lpVersion) p
        lpszComment' <- (#peek WSAQUERYSETW, lpszComment) p
        dwNameSpace' <- (#peek WSAQUERYSETW, dwNameSpace) p
		lpNSProviderId' <- (#peek WSAQUERYSETW, lpNSProviderId) p
		lpszContext' <- (#peek WSAQUERYSETW, lpszContext) p
		dwNumberOfProtocols' <- (#peek WSAQUERYSETW, dwNumberOfProtocols) p
		lpafpProtocols' <- (#peek WSAQUERYSETW, lpafpProtocols) p
		lpszQueryString' <- (#peek WSAQUERYSETW, lpszQueryString) p
		dwNumberOfCsAddrs' <- (#peek WSAQUERYSETW, dwNumberOfCsAddrs) p
		lpcsaBuffer' <- (#peek WSAQUERYSETW, lpcsaBuffer) p
		dwOutputFlags' <- (#peek WSAQUERYSETW, dwOutputFlags) p
		lpBlob' <- (#peek WSAQUERYSETW, lpBlob) p
        return $ WSAQuerySet dwSize' lpszServiceInstanceName' lpServiceClassId' lpVersion' lpszComment' dwNameSpace' lpNSProviderId' lpszContext' dwNumberOfProtocols' lpafpProtocols' lpszQueryString' dwNumberOfCsAddrs' lpcsaBuffer' dwOutputFlags' lpBlob'
    poke p (WSAQuerySet dwSize' lpszServiceInstanceName' lpServiceClassId' lpVersion' lpszComment' dwNameSpace' lpNSProviderId' lpszContext' dwNumberOfProtocols' lpafpProtocols' lpszQueryString' dwNumberOfCsAddrs' lpcsaBuffer' dwOutputFlags' lpBlob') = do
        (#poke WSAQUERYSETW, dwSize) p dwSize'
        (#poke WSAQUERYSETW, lpszServiceInstanceName) p lpszServiceInstanceName'
        (#poke WSAQUERYSETW, lpServiceClassId) p lpServiceClassId'
        (#poke WSAQUERYSETW, lpVersion) p lpVersion'
        (#poke WSAQUERYSETW, lpszComment) p lpszComment'
        (#poke WSAQUERYSETW, dwNameSpace) p dwNameSpace'
		(#poke WSAQUERYSETW, lpNSProviderId) p lpNSProviderId'
		(#poke WSAQUERYSETW, lpszContext) p lpszContext'
		(#poke WSAQUERYSETW, dwNumberOfProtocols) p dwNumberOfProtocols'
		(#poke WSAQUERYSETW, lpafpProtocols) p lpafpProtocols'
		(#poke WSAQUERYSETW, lpszQueryString) p lpszQueryString'
		(#poke WSAQUERYSETW, dwNumberOfCsAddrs) p dwNumberOfCsAddrs'
		(#poke WSAQUERYSETW, lpcsaBuffer) p lpcsaBuffer'
		(#poke WSAQUERYSETW, dwOutputFlags) p dwOutputFlags'
		(#poke WSAQUERYSETW, lpBlob) p lpBlob'

type Socket = CUInt

data ServerInfo = ServerInfo {
    client :: Socket,
    serverSock :: Socket,
    session :: Ptr WSAQuerySet
}

instance Storable ServerInfo where
    sizeOf _ = (#size server_info_t)
    alignment _ = alignment (undefined :: Socket)
    peek p = do
        client' <- (#peek server_info_t, client) p
        serverSock' <- (#peek server_info_t, serverSock) p
        session' <- (#peek server_info_t, session) p
        return $ ServerInfo client' serverSock' session'
    poke p (ServerInfo client' serverSock' session') = do
        (#poke server_info_t, client) p client'
        (#poke server_info_t, serverSock) p serverSock'
        (#poke server_info_t, session) p session'

foreign import ccall "init_server" init_server :: IO ()
foreign import ccall "read_server" read_server :: Ptr ServerInfo -> IO CString
foreign import ccall "write_server" write_server :: Ptr ServerInfo -> CString -> IO ()
foreign import ccall "close_server" close_server :: Ptr ServerInfo -> IO ()