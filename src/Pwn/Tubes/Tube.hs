module Pwn.Tubes.Tube where

import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (($$))
import           Data.Conduit.Binary          (sinkHandle, sourceHandle)
import           System.IO

import           Pwn.Log
import           Util                         (eofError)

class Tube a where
  inputHandle  :: a -> Handle
  outputHandle :: a -> Handle

  wait  :: a -> IO ()
  close :: a -> IO ()
  shutdown :: a -> IO ()

recv :: (Tube a) => a -> IO BS.ByteString
recv tube = BS.hGetSome (outputHandle tube) 4096

recvn :: (Tube a) => a -> Int -> IO BS.ByteString
recvn tube len = do
  r <- BS.hGet (outputHandle tube) len
  if BS.length r < len then eofError (outputHandle tube) "recvn"
                       else return r

send :: (Tube a) => a -> BS.ByteString -> IO ()
send tube = BS.hPut (inputHandle tube)

recvline :: (Tube a) => a -> IO BS.ByteString
recvline tube = recvline' BS.empty
  where recvline' buf = do
          newbuf <- BS.append buf <$> recvn tube 1
          case BS.last newbuf of
               '\n' -> return newbuf
               _    -> recvline' newbuf

recvuntil :: (Tube a) => a -> BS.ByteString -> IO BS.ByteString
recvuntil tube suff = recvuntil' BS.empty
  where recvuntil' buf = do
          newbuf <- BS.append buf <$> recvn tube 1
          if BS.isSuffixOf suff newbuf then return newbuf
                                       else recvuntil' newbuf

sendline :: (Tube a) => a -> BS.ByteString -> IO ()
sendline tube = send tube . appendNL
  where appendNL = flip BS.snoc '\n'

interactive :: (Tube a) => a -> IO ()
interactive tube = do
  info "Entering interactive mode"
  runResourceT $ do
    (rthread, _) <- allocate(forkIO $ runResourceT $ do
      sourceHandle (outputHandle tube) $$ sinkHandle stdout
      liftIO $ info "Connection closed") killThread
    lift $ sourceHandle stdin $$ sinkHandle (inputHandle tube)
    release rthread
  info "Leaving interactive mode"
