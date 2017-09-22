module Pwn.Tubes.Socket
  ( Socket (address, portNumber)
  , remote
  ) where

import           Data.Monoid            ((<>))
import qualified Network.Socket         as NS
import           System.IO

import           Pwn.Log
import qualified Pwn.Tubes.Tube         as T

data Socket = Socket { address      :: String
                     , portNumber   :: Int
                     , socketHandle :: Handle
                     }

instance T.Tube Socket where
  inputHandle  = socketHandle
  outputHandle = socketHandle
  wait         = wait
  close        = close
  shutdown     = shutdown

remote :: String -> Int -> IO Socket
remote addr port = do
  let logstr = "Opening connection to " <> addr <> " on port " <> show port
  status logstr
  ai <- head <$> NS.getAddrInfo Nothing (Just addr) (Just $ show port)
  sock <- NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress ai)
  hsock <- NS.socketToHandle sock ReadWriteMode
  hSetBuffering hsock NoBuffering
  success $ logstr <> ": Done"
  return $ Socket addr port hsock

wait :: Socket -> IO ()
wait _ = error "not implemented yet"

close :: Socket -> IO ()
close = hClose . socketHandle

shutdown :: Socket -> IO ()
shutdown = close
