module Pwn.Tubes.Socket
  ( Socket (address, portNumber)
  , remote
  ) where

import           Control.Monad.IO.Class
import           Data.Monoid            ((<>))
import qualified Network.Socket         as NS
import           System.IO

import           Pwn.Config
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

remote :: MonadPwn m => String -> Int -> m Socket
remote addr port = do
  let logstr = "Opening connection to " <> addr <> " on port " <> show port
  status logstr
  ai <- liftIO $ head <$> NS.getAddrInfo Nothing (Just addr) (Just $ show port)
  sock <- liftIO $ NS.socket (NS.addrFamily ai) NS.Stream NS.defaultProtocol
  liftIO $ NS.connect sock (NS.addrAddress ai)
  hsock <- liftIO $ NS.socketToHandle sock ReadWriteMode
  liftIO $ hSetBuffering hsock NoBuffering
  success $ logstr <> ": Done"
  return $ Socket addr port hsock

wait :: MonadPwn m => Socket -> m ()
wait _ = error "not implemented yet"

close :: MonadPwn m => Socket -> m ()
close = liftIO . hClose . socketHandle

shutdown :: MonadPwn m => Socket -> m ()
shutdown = close
