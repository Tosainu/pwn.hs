{-# LANGUAGE RecordWildCards #-}

module Pwn.Tubes.Socket
  ( Socket (..)
  , remote
  ) where

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
  inputHandle  = hsocket
  outputHandle = hsocket
  wait         = wait
  close        = close
  shutdown     = shutdown

remote :: String -> Int -> IO Socket
remote address port = do
  let logstr = "Opening connection to " <> address <> " on port " <> show port
  status logstr
  ai <- head <$> NS.getAddrInfo Nothing (Just address) (Just $ show port)
  sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress ai)
  hsocket <- NS.socketToHandle sock ReadWriteMode
  hSetBuffering hsocket NoBuffering
  success $ logstr <> ": Done"
  return Socket {..}

wait :: Socket -> IO ()
wait _ = error "not implemented yet"

close :: Socket -> IO ()
close = hClose . hsocket

shutdown :: Socket -> IO ()
shutdown = close
