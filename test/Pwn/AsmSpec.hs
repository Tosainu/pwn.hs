{-# LANGUAGE OverloadedStrings #-}

module Pwn.AsmSpec
  ( main
  , spec
  ) where

import           Control.Monad.Reader (runReaderT)
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
      ret <- runReaderT (asm src) linux32Config
      ret `shouldBe` (Right bin)

    it "assemble x86-64 code" $ do
      let src = "xor rdi, rdi ; mov rax, 0x3c ; syscall"
          bin = "\x48\x31\xff\x48\xc7\xc0\x3c\x00\x00\x00\x0f\x05"
      ret <- runReaderT (asm src) linux64Config
      ret `shouldBe` (Right bin)

    it "assemble invalid code" $ do
      ret1 <- runReaderT (asm "nyanyanya") linux32Config
      (isLeft ret1) `shouldBe` True

      ret2 <- runReaderT (asm "nyanyanya") linux64Config
      (isLeft ret2) `shouldBe` True

    it "assemble with invalid config" $ do
      ret <- runReaderT (asm "nop") invalidConfig
      (isLeft ret) `shouldBe` True

  describe "Pwn.Asm.disasm" $ do
    it "disassemble i386 code" $ do
      pending

    it "disassemble x86-64 code" $ do
      pending
