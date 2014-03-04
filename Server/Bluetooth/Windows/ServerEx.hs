module ServerEx where

import qualified WinBluetoothServer as WB
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc

main :: IO ()
main = do
    info <- WB.init_server
    loopServer info
  
loopServer :: Ptr WB.ServerInfo -> IO ()
loopServer info = do
    cMessage <- WB.read_server info
    hsMessage <- peekCString cMessage
    free cMessage
    if (not $ null hsMessage)
       then do
           response <- newCString $ hermitMagic hsMessage
           WB.write_server info response
           free response
           loopServer info
       else do
           WB.close_server info 

hermitMagic :: String -> String
-- Replace this with some other crazy string manipulation
hermitMagic str = str ++ " (don't forget Haskell!)"