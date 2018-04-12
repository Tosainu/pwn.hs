module Pwn.Tubes.Tube where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Monad           (when)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8   as BS
import qualified Data.Conduit            as C
import qualified Data.Conduit.Binary     as C (sinkHandle, sourceHandle)
import           System.IO               (stdin, stdout)

import           Pwn.Config
import           Pwn.Log

class Tube a where
  recv     :: MonadPwn m => a -> m BS.ByteString
  recvn    :: MonadPwn m => a -> Int -> m BS.ByteString
  send     :: MonadPwn m => a -> BS.ByteString -> m ()
  isEOF    :: MonadPwn m => a -> m Bool
  source   :: a -> C.ConduitT () BS.ByteString IO ()
  sink     :: a -> C.ConduitT BS.ByteString C.Void IO ()
  wait     :: MonadPwn m => a -> m ()
  close    :: MonadPwn m => a -> m ()
  shutdown :: MonadPwn m => a -> m ()

recvline :: (MonadPwn m, Tube a) => a -> m BS.ByteString
recvline tube = recvuntil tube $ BS.singleton '\n'

recvuntil :: (MonadPwn m, Tube a) => a -> BS.ByteString -> m BS.ByteString
recvuntil tube suff = recvuntil' BS.empty
  where recvuntil' buf = do
          newbuf <- BS.append buf <$> recvn tube 1
          if BS.isSuffixOf suff newbuf then return newbuf
                                       else recvuntil' newbuf

sendline :: (MonadPwn m, Tube a) => a -> BS.ByteString -> m ()
sendline tube = send tube . appendNL
  where appendNL = flip BS.snoc '\n'

interactive :: (MonadPwn m, Tube a) => a -> m ()
interactive tube = do
  info "Entering interactive mode"
  r <- liftIO $ do
    mv <- newEmptyMVar
    rx <- forkIO $ do
      source tube `C.connect` C.sinkHandle stdout
      putMVar mv True
    tx <- forkIO $ do
      C.sourceHandle stdin `C.connect` sink tube
      putMVar mv False
    takeMVar mv <* mapM_ killThread [rx, tx]
  when r $ warning "Connection may be closed"
  info "Leaving interactive mode"
