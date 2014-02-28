{-|
    
This file describes Haskell's interface to bluez_server.h, from which it calls the functions that deal with the Linux BlueZ libraries. This uses some markup from the hsc2hs utility to make it easier to marshal and unmarshal Haskell data types into structs. To build a .hs file from this, run the following command in a terminal:

    hsc2hs BlueZServer.chs

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module BlueZServer where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "bluez_server.h"

data SDPSession = SDPSession {
    sock :: CInt,
    state :: CInt,
    local :: CInt,
    flags :: CInt,
    tid :: CUInt, -- unsigned short int
    priv :: Ptr () -- void*
}

instance Storable SDPSession where
    sizeOf _ = (#size sdp_session_t)
    alignment _ = alignment (undefined :: CInt)
    peek p = do
        sock' <- (#peek sdp_session_t, sock) p
        state' <- (#peek sdp_session_t, state) p
        local' <- (#peek sdp_session_t, local) p
        flags' <- (#peek sdp_session_t, flags) p
        tid' <- (#peek sdp_session_t, tid) p
        priv' <- (#peek sdp_session_t, priv) p
        return $ SDPSession sock' state' local' flags' tid' priv'
    poke p (SDPSession sock' state' local' flags' tid' priv') = do
        (#poke sdp_session_t, sock) p sock'
        (#poke sdp_session_t, state) p state'
        (#poke sdp_session_t, local) p local'
        (#poke sdp_session_t, flags) p flags'
        (#poke sdp_session_t, tid) p tid'
        (#poke sdp_session_t, priv) p priv'

data ServerInfo = ServerInfo {
    client :: CInt,
    serverSock :: CInt,
    session :: Ptr SDPSession
}

instance Storable ServerInfo where
    sizeOf _ = (#size server_info_t)
    alignment _ = alignment (undefined :: CInt)
    peek p = do
        client' <- (#peek server_info_t, client) p
        serverSock' <- (#peek server_info_t, serverSock) p
        session' <- (#peek server_info_t, session) p
        return $ ServerInfo client' serverSock' session'
    poke p (ServerInfo client' serverSock' session') = do
        (#poke server_info_t, client) p client'
        (#poke server_info_t, serverSock) p serverSock'
        (#poke server_info_t, session) p session'

foreign import ccall "init_server" init_server :: IO (Ptr ServerInfo)
foreign import ccall "read_server" read_server :: Ptr ServerInfo -> CString -> IO CString
foreign import ccall "write_server" write_server :: Ptr ServerInfo -> CString -> IO ()
foreign import ccall "close_server" close_server :: Ptr ServerInfo -> IO ()