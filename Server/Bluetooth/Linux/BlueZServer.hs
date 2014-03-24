{-# LINE 1 "BlueZServer.chs" #-}
{-|
{-# LINE 2 "BlueZServer.chs" #-}
    
This file describes Haskell's interface to bluez_server.h, from which it calls the functions that deal with the Linux BlueZ libraries. This uses some markup from the hsc2hs utility to make it easier to marshal and unmarshal Haskell data types into structs. To build a .hs file from this, run the following command in a terminal:

    hsc2hs BlueZServer.chs

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module BlueZServer where
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


{-# LINE 18 "BlueZServer.chs" #-}

{-# LINE 19 "BlueZServer.chs" #-}

{-# LINE 20 "BlueZServer.chs" #-}

data SDPSession = SDPSession {
    sock :: CInt,
    state :: CInt,
    local :: CInt,
    flags :: CInt,
    tid :: CUInt, -- unsigned short int
    priv :: Ptr () -- void*
}

instance Storable SDPSession where
    sizeOf _ = ((32))
{-# LINE 32 "BlueZServer.chs" #-}
    alignment _ = alignment (undefined :: CInt)
    peek p = do
        sock' <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 35 "BlueZServer.chs" #-}
        state' <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 36 "BlueZServer.chs" #-}
        local' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 37 "BlueZServer.chs" #-}
        flags' <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 38 "BlueZServer.chs" #-}
        tid' <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 39 "BlueZServer.chs" #-}
        priv' <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 40 "BlueZServer.chs" #-}
        return $ SDPSession sock' state' local' flags' tid' priv'
    poke p (SDPSession sock' state' local' flags' tid' priv') = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p sock'
{-# LINE 43 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p state'
{-# LINE 44 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p local'
{-# LINE 45 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p flags'
{-# LINE 46 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p tid'
{-# LINE 47 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24)) p priv'
{-# LINE 48 "BlueZServer.chs" #-}

data ServerInfo = ServerInfo {
    client :: CInt,
    serverSock :: CInt,
    session :: Ptr SDPSession
}

instance Storable ServerInfo where
    sizeOf _ = ((16))
{-# LINE 57 "BlueZServer.chs" #-}
    alignment _ = alignment (undefined :: CInt)
    peek p = do
        client' <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 60 "BlueZServer.chs" #-}
        serverSock' <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 61 "BlueZServer.chs" #-}
        session' <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 62 "BlueZServer.chs" #-}
        return $ ServerInfo client' serverSock' session'
    poke p (ServerInfo client' serverSock' session') = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p client'
{-# LINE 65 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p serverSock'
{-# LINE 66 "BlueZServer.chs" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p session'
{-# LINE 67 "BlueZServer.chs" #-}

foreign import ccall "init_server" init_server :: IO (Ptr ServerInfo)
foreign import ccall "read_server" read_server :: Ptr ServerInfo -> IO CString
foreign import ccall "write_server" write_server :: Ptr ServerInfo -> CString -> IO ()
foreign import ccall "close_server" close_server :: Ptr ServerInfo -> IO ()