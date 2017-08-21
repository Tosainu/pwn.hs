module Pwn.Tubes.Socket
  ( Socket (..)
  , remote
  ) where

import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString.Char8     (ByteString)
import           Data.Monoid               ((<>))
import qualified Network.Socket            as NS hiding (recv)
import qualified Network.Socket.ByteString as NS

import           Pwn.Log
import qualified Pwn.Tubes.Tube            as T

data Socket = Socket { address :: String
                     , port    :: Int
                     , socket  :: NS.Socket
                     }

instance T.Tube Socket where
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
  liftIO $ success $ logstr <> ": Done"
  return $ Socket a p sock

recvn :: Socket -> Int -> IO ByteString
recvn sock len = NS.recv (socket sock) len

send :: Socket -> ByteString -> IO ()
send sock str = NS.sendAll (socket sock) str

wait :: Socket -> IO ()
wait _ = error "not implemented yet"

close :: Socket -> IO ()
close = NS.close . socket

shutdown :: Socket -> IO ()
shutdown sock = NS.shutdown (socket sock) NS.ShutdownBoth

interactive :: Socket -> IO ()
interactive _ = error "not implemented yet"
