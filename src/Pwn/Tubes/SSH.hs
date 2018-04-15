{-# LANGUAGE LambdaCase #-}

module Pwn.Tubes.SSH
  ( SSHProcess
  , defaultSSHConfig
  , ssh
  , sshWith
  , closeSSH
  , sshProcess
  , sshShell
  , closeChannel
  ) where

import           Control.Monad                      (when)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8              as BS
import qualified Data.Conduit                       as C
import           Data.Monoid                        ((<>))
import qualified Network.SSH.Client.LibSSH2         as S
import qualified Network.SSH.Client.LibSSH2.Foreign as S

import           Pwn.Config
import           Pwn.Log
import qualified Pwn.Tubes.Tube                     as T

data SSHProcess = SSHProcess { channel :: S.Channel }

instance T.Tube SSHProcess where
  recv s    = T.recvn s 4096
  recvn s n = liftIO $ S.readChannel (channel s) (fromIntegral n)
  send s    = liftIO . S.writeChannel (channel s)
  isEOF s   = liftIO $ S.channelIsEOF (channel s)
  source    = sourceChannel . channel
  sink      = sinkChannel . channel
  wait s    = liftIO $ S.channelWaitEOF (channel s)
  close     = closeChannel . channel
  shutdown  = T.close

data SSHConfig = SSHConfig
               { port     :: Int
               , password :: String
               , keyFile  :: Maybe (FilePath, FilePath)
               }

defaultSSHConfig :: SSHConfig
defaultSSHConfig = SSHConfig
  { port     = 22
  , password = ""
  , keyFile  = Nothing
  }

ssh :: MonadPwn m => String -> String -> m S.Session
ssh name host = sshWith name host defaultSSHConfig

sshWith :: MonadPwn m => String -> String -> SSHConfig -> m S.Session
sshWith name host cfg = do
  let logstr = "Connecting to " <> host <> " on port " <> show (port cfg)
  status logstr
  s <- liftIO $ S.sessionInit host (port cfg)
  liftIO $ auth s name (password cfg) (keyFile cfg)
  success $ logstr <> ": Done"
  return s
  where
    auth s n p (Just (pub, pri)) = S.publicKeyAuthFile s n pub pri p
    auth s n p Nothing           = S.usernamePasswordAuth s n p

closeSSH :: MonadPwn m => S.Session -> m ()
closeSSH = liftIO . S.sessionClose

sshProcess :: MonadPwn m => S.Session -> [String] -> m SSHProcess
sshProcess s c = do
  let logstr = "Starting process '" <> unwords c <> "'"
  status logstr
  ch <- liftIO $ S.openChannelSession s
  liftIO $ S.channelExecute ch (unwords c)
  success $ logstr <> ": Done"
  return $ SSHProcess ch

sshShell :: MonadPwn m => S.Session -> m SSHProcess
sshShell s = do
  status "Opening shell"
  liftIO $ do
    ch <- S.openChannelSession s
    S.requestPTY ch "linux"
    S.channelShell ch
    return $ SSHProcess ch

closeChannel :: MonadPwn m => S.Channel -> m ()
closeChannel c = liftIO $ S.closeChannel c >> S.freeChannel c

sourceChannel :: S.Channel -> C.ConduitT () BS.ByteString IO ()
sourceChannel = sourceChannel'
  where
    sourceChannel' c = do
      res <- liftIO $ S.readChannel c 4096
      when (BS.length res > 0) $ do
        C.yield res
        sourceChannel' c

sinkChannel :: S.Channel -> C.ConduitT BS.ByteString C.Void IO ()
sinkChannel = sinkChannel'
  where
    sinkChannel' c = C.await >>= \case
      Nothing -> return ()
      Just s  -> do
        liftIO $ S.writeChannel c s
        sinkChannel' c
