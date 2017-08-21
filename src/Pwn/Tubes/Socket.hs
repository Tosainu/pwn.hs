module Pwn.Tubes.Socket
  ( Socket (..)
  , remote
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Monoid            ((<>))
import qualified Network.Socket         as NS
import           System.IO

import           Pwn.Log
import qualified Pwn.Tubes.Tube         as T

data Socket = Socket { address :: String
                     , port    :: Int
                     , hsocket :: Handle
                     }

instance T.Tube Socket where
  recv  = recv
  recvn = recvn
  send  = send
  wait  = wait
  close = close
  shutdown = shutdown
  interactive = interactive

remote :: String -> Int -> IO Socket
remote a p = do
  let logstr = "Opening connection to " <> a <> " on port " <> show p
  liftIO $ status logstr
  ai <- head <$> NS.getAddrInfo Nothing (Just a) (Just $ show p)
  sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress ai)
  h <- NS.socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  liftIO $ success $ logstr <> ": Done"
  return $ Socket a p h

recv :: Socket -> IO ByteString
recv sock = BS.hGetSome (hsocket sock) 4096

recvn :: Socket -> Int -> IO ByteString
recvn sock len = BS.hGet (hsocket sock) len

send :: Socket -> ByteString -> IO ()
send sock str = BS.hPut (hsocket sock) str

wait :: Socket -> IO ()
wait _ = error "not implemented yet"

close :: Socket -> IO ()
close = hClose . hsocket

shutdown :: Socket -> IO ()
shutdown = close

interactive :: Socket -> IO ()
interactive _ = error "not implemented yet"
