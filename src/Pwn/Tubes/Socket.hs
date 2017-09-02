{-# LANGUAGE RecordWildCards #-}

module Pwn.Tubes.Socket
  ( Socket (..)
  , remote
  ) where

import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (($$))
import           Data.Conduit.Binary          (sinkHandle, sourceHandle)
import           Data.Monoid                  ((<>))
import qualified Network.Socket               as NS
import           System.IO

import           Pwn.Log
import qualified Pwn.Tubes.Tube               as T

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
remote address port = do
  let logstr = "Opening connection to " <> address <> " on port " <> show port
  liftIO $ status logstr
  ai <- head <$> NS.getAddrInfo Nothing (Just address) (Just $ show port)
  sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress ai)
  hsocket <- NS.socketToHandle sock ReadWriteMode
  hSetBuffering hsocket NoBuffering
  liftIO $ success $ logstr <> ": Done"
  return Socket {..}

recv :: Socket -> IO ByteString
recv sock = BS.hGetSome (hsocket sock) 4096

recvn :: Socket -> Int -> IO ByteString
recvn sock = BS.hGet (hsocket sock)

send :: Socket -> ByteString -> IO ()
send sock = BS.hPut (hsocket sock)

wait :: Socket -> IO ()
wait _ = error "not implemented yet"

close :: Socket -> IO ()
close = hClose . hsocket

shutdown :: Socket -> IO ()
shutdown = close

interactive :: Socket -> IO ()
interactive Socket { hsocket = hsock } = do
  info "Entering interactive mode"
  runResourceT $ do
    (rthread, _) <- allocate(forkIO $ runResourceT $ do
      sourceHandle hsock $$ sinkHandle stdout
      liftIO $ info "Connection closed") killThread
    lift $ sourceHandle stdin $$ sinkHandle hsock
    release rthread
  info "Leaving interactive mode"
