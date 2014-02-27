module ServerEx where

import qualified BluezServer
import Control.Monad
import Foreign.C

main :: IO ()
main = BluezServer.init_server >>= \ client -> loopServer client
  
loopServer :: CInt -> IO ()
loopServer client = do
  cMessage <- BluezServer.read_server client
  message <- peekCString cMessage
  response <- newCString $ hermitMagic message
  when (not $ null message) $ do
    BluezServer.write_server client response
    loopServer client

hermitMagic :: [Char] -> [Char]
-- Replace this with some other crazy string manipulation
hermitMagic str = str ++ " (don't forget Haskell!)"