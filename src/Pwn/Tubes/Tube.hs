module Pwn.Tubes.Tube where

import           Data.ByteString.Char8 (ByteString)

class Tube a where
  recvn :: a -> Int -> IO ByteString
  send  :: a -> ByteString -> IO ()

  wait  :: a -> IO ()
  close :: a -> IO ()
  shutdown :: a -> IO ()

  interactive :: a -> IO ()
