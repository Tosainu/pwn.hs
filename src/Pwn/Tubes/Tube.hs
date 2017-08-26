module Pwn.Tubes.Tube where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

class Tube a where
  recv  :: a -> IO ByteString
  recvn :: a -> Int -> IO ByteString
  send  :: a -> ByteString -> IO ()

  wait  :: a -> IO ()
  close :: a -> IO ()
  shutdown :: a -> IO ()

  interactive :: a -> IO ()

recvline :: (Tube a) => a -> IO ByteString
recvline tube = recvline' BS.empty
  where recvline' buf = do
          newbuf <- BS.append buf <$> recvn tube 1
          case BS.last newbuf of
               '\n' -> return newbuf
               _    -> recvline' newbuf

recvuntil :: (Tube a) => a -> ByteString -> IO ByteString
recvuntil tube suff = recvuntil' BS.empty
  where recvuntil' buf = do
          newbuf <- BS.append buf <$> recvn tube 1
          if BS.isSuffixOf suff newbuf then return newbuf
                                       else recvuntil' newbuf

sendline :: (Tube a) => a -> ByteString -> IO ()
sendline tube = send tube . appendNL
  where appendNL = flip BS.snoc '\n'
