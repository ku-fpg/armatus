module ServerEx where

import qualified BlueZServer as BZ
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc

main :: IO ()
main = do
    info <- BZ.init_server
    loopServer info
  
loopServer :: Ptr BZ.ServerInfo -> IO ()
loopServer info = do
    messagePtr <- newCString ""
    cMessage <- BZ.read_server info messagePtr
    message <- peekCString cMessage
    free messagePtr
    if (not $ null message)
       then do
           response <- newCString $ hermitMagic message
           BZ.write_server info response
           free response
           loopServer info
       else do
           BZ.close_server info 

hermitMagic :: String -> String
-- Replace this with some other crazy string manipulation
hermitMagic str = str ++ " (don't forget Haskell!)"