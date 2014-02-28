{-# LANGUAGE ForeignFunctionInterface #-}
module BluezServer where
import Foreign.C
import Foreign.Storable

foreign import ccall "init_server" init_server :: IO CInt
foreign import ccall "read_server" read_server :: CInt -> CString -> IO CString
foreign import ccall "write_server" write_server :: CInt -> CString -> IO ()
foreign import ccall "close_server" close_server :: CInt -> CInt -> SDPSession -> IO ()