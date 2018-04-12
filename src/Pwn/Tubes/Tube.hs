module Pwn.Tubes.Tube where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Monad           (when)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8   as BS
import           Data.Conduit            (connect)
import           Data.Conduit.Binary     (sinkHandle, sourceHandle)
import           System.IO

import           Pwn.Config
import           Pwn.Log
import           Util                    (eofError)

class Tube a where
  inputHandle  :: a -> Handle
  outputHandle :: a -> Handle

  wait  :: MonadPwn m => a -> m ()
  close :: MonadPwn m => a -> m ()
  shutdown :: MonadPwn m => a -> m ()

recv :: (MonadPwn m, Tube a) => a -> m BS.ByteString
recv tube = liftIO $ BS.hGetSome (outputHandle tube) 4096

recvn :: (MonadPwn m, Tube a) => a -> Int -> m BS.ByteString
recvn tube len = liftIO $ do
  r <- BS.hGet (outputHandle tube) len
  if BS.length r < len then eofError (outputHandle tube) "recvn"
                       else return r

send :: (MonadPwn m, Tube a) => a -> BS.ByteString -> m ()
send tube = liftIO . BS.hPut (inputHandle tube)

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
      sourceHandle (outputHandle tube) `connect` sinkHandle stdout
      putMVar mv True
    tx <- forkIO $ do
      sourceHandle stdin `connect` sinkHandle (inputHandle tube)
      putMVar mv False
    takeMVar mv <* mapM_ killThread [rx, tx]
  when r $ warning "Connection may be closed"
  info "Leaving interactive mode"
