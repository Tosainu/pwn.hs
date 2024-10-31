{-# LANGUAGE OverloadedStrings #-}

module Pwn.AsmSpec
  ( main
  , spec
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Pwn.Asm
import           Pwn.Config
import           Test.Hspec

main :: IO ()
main = hspec spec

linux32Config :: Config
linux32Config = Config { arch   = "i386"
                       , bits   = 32
                       , endian = Little
                       , os     = "linux"
                       }

linux64Config :: Config
linux64Config = Config { arch   = "amd64"
                       , bits   = 64
                       , endian = Little
                       , os     = "linux"
                       }

invalidConfig :: Config
invalidConfig = Config { arch = "poe"
                       , bits = -1
                       , endian = Little
                       , os = "unknown"
                       }

spec :: Spec
spec = do
  describe "Pwn.Asm.asm" $ do
    it "assemble i386 code" $ do
      let src = "xor ebx, ebx ; mov eax, 0x1 ; int 0x80"
          bin = "\x31\xdb\xb8\x01\x00\x00\x00\xcd\x80"
      ret <- pwnWith linux64Config $ asm src
      ret `shouldBe` Right bin

    it "assemble x86-64 code" $ do
      let src = "xor rdi, rdi ; mov rax, 0x3c ; syscall"
          bin = "\x48\x31\xff\x48\xc7\xc0\x3c\x00\x00\x00\x0f\x05"
      ret <- pwnWith linux64Config $ asm src
      ret `shouldBe` Right bin

    it "assemble invalid code" $ do
      ret1 <- pwnWith linux32Config $ asm "nyanyanya"
      isLeft ret1 `shouldBe` True

      ret2 <- pwnWith linux64Config $ asm "nyanyanya"
      isLeft ret2 `shouldBe` True

    it "assemble with invalid config" $ do
      ret <- pwnWith invalidConfig $ asm "nop"
      isLeft ret `shouldBe` True

  describe "Pwn.Asm.disasm" $ do
    it "disassemble i386 code" $ do
      let bin = "\x31\xdb\xb8\x01\x00\x00\x00\xcd\x80"
      Right ret <- pwnWith linux32Config $ disasm bin
      let retl = BS.lines ret
      length retl `shouldBe` 4
      "31 db"                `BS.isInfixOf` (retl !! 1) `shouldBe` True
      "b8 01 00 00 00"       `BS.isInfixOf` (retl !! 2) `shouldBe` True
      "cd 80"                `BS.isInfixOf` (retl !! 3) `shouldBe` True

    it "disassemble x86-64 code" $ do
      let bin = "\x48\x31\xff\x48\xc7\xc0\x3c\x00\x00\x00\x0f\x05"
      Right ret <- pwnWith linux64Config $ disasm bin
      let retl = BS.lines ret
      length retl `shouldBe` 4
      "48 31 ff"             `BS.isInfixOf` (retl !! 1) `shouldBe` True
      "48 c7 c0 3c 00 00 00" `BS.isInfixOf` (retl !! 2) `shouldBe` True
      "0f 05"                `BS.isInfixOf` (retl !! 3) `shouldBe` True

    it "disassemble with invalid config" $ do
      ret <- pwnWith invalidConfig $ disasm "'x90"
      isLeft ret `shouldBe` True
