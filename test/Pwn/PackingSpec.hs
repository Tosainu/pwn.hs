module Pwn.PackingSpec
  ( main
  , spec
  ) where

import qualified Data.ByteString.Char8 as BS
import           Pwn.Packing
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Pwn.Packing.pack" $ do
    it "pack 32 Little 0x12345678" $ do
      pack 32 Little (0x12345678 :: Int) `shouldBe` Just (BS.pack "\x78\x56\x34\x12")

    it "pack 32 Big 0x12345678" $ do
      pack 32 Big (0x12345678 :: Int) `shouldBe` Just (BS.pack "\x12\x34\x56\x78")

    it "pack 64 Little 0x1234567890123456" $ do
      pack 64 Little (0x1234567890123456 :: Int) `shouldBe` Just (BS.pack "\x56\x34\x12\x90\x78\x56\x34\x12")

    it "pack 64 Big 0x1234567890123456" $ do
      pack 64 Big (0x1234567890123456 :: Int) `shouldBe` Just (BS.pack "\x12\x34\x56\x78\x90\x12\x34\x56")

    it "pack with worng bits" $ do
      pack 31 Little (0x12345678 :: Int) `shouldBe` Nothing
      pack (-1) Little (0x12345678 :: Int) `shouldBe` Nothing

  describe "Pwn.Packing.p32" $ do
    it "p32 0x12345678" $ do
      p32 0x12345678 `shouldBe` Just (BS.pack "\x78\x56\x34\x12")

    it "p32 0x12" $ do
      p32 0x12 `shouldBe` Just (BS.pack "\x12\x00\x00\x00")

  describe "Pwn.Packing.p32be" $ do
    it "p32be 0x12345678" $ do
      p32be 0x12345678 `shouldBe` Just (BS.pack "\x12\x34\x56\x78")

    it "p32be 0x12" $ do
      p32be 0x12 `shouldBe` Just (BS.pack "\x00\x00\x00\x12")

  describe "Pwn.Packing.p64" $ do
    it "p64 0x1234567890123456" $ do
      p64 0x1234567890123456 `shouldBe` Just (BS.pack "\x56\x34\x12\x90\x78\x56\x34\x12")

    it "p64 0x12" $ do
      p64 0x12 `shouldBe` Just (BS.pack "\x12\x00\x00\x00\x00\x00\x00\x00")

  describe "Pwn.Packing.p64be" $ do
    it "p64be 0x1234567890123456" $ do
      p64be 0x1234567890123456 `shouldBe` Just (BS.pack "\x12\x34\x56\x78\x90\x12\x34\x56")

    it "p64be 0x12" $ do
      p64be 0x12 `shouldBe` Just (BS.pack "\x00\x00\x00\x00\x00\x00\x00\x12")

  describe "Pwn.Packing.unpack" $ do
    it "unpack 32 Little \"\\x78\\x56\\x34\\x12\"" $ do
      unpack 32 Little (BS.pack "\x78\x56\x34\x12") `shouldBe` Just (0x12345678 :: Int)

    it "unpack 32 Big \"\\x12\\x34\\x56\\x78\"" $ do
      unpack 32 Big (BS.pack "\x12\x34\x56\x78") `shouldBe` Just (0x12345678 :: Int)

    it "unpack 64 Little \"\\x56\\x34\\x12\\x90\\x78\\x56\\x34\\x12\"" $ do
      unpack 64 Little (BS.pack "\x56\x34\x12\x90\x78\x56\x34\x12") `shouldBe` Just (0x1234567890123456 :: Int)

    it "unpack 64 Big \"\\x12\\x34\\x56\\x78\\x90\\x12\\x34\\x56\"" $ do
      unpack 64 Big (BS.pack "\x12\x34\x56\x78\x90\x12\x34\x56") `shouldBe` Just (0x1234567890123456 :: Int)

    it "unpack with worng bits" $ do
      unpack 31 Little (BS.pack "\x78\x56\x34\x12") `shouldBe` (Nothing :: Maybe Int)
      unpack (-1) Little (BS.pack "\x78\x56\x34\x12") `shouldBe` (Nothing :: Maybe Int)

    it "unpack with worng length" $ do
      unpack 32 Little (BS.replicate 10 'A') `shouldBe` (Nothing :: Maybe Int)
      unpack 64 Little (BS.replicate 10 'A') `shouldBe` (Nothing :: Maybe Int)

  describe "Pwn.Packing.u32" $ do
    it "u32 \"\\x78\\x56\\x34\\x12\"" $ do
      u32 (BS.pack "\x78\x56\x34\x12") `shouldBe` Just 0x12345678

    it "u32 \"\\x12\\x00\\x00\\x00\"" $ do
      u32 (BS.pack "\x12\x00\x00\x00") `shouldBe` Just 0x12

    it "u32 with worng length" $ do
      u32 (BS.replicate 10 'A') `shouldBe` Nothing

  describe "Pwn.Packing.u32be" $ do
    it "u32be \"\\x12\\x34\\x56\\x78\"" $ do
      u32be (BS.pack "\x12\x34\x56\x78") `shouldBe` Just 0x12345678

    it "u32be \"\\x00\\x00\\x00\\x12\"" $ do
      u32be (BS.pack "\x00\x00\x00\x12") `shouldBe` Just 0x12

    it "u32be with worng length" $ do
      u32be (BS.replicate 10 'A') `shouldBe` Nothing

  describe "Pwn.Packing.u64" $ do
    it "u64 \"\\x56\\x34\\x12\\x90\\x78\\x56\\x34\\x12\"" $ do
      u64 (BS.pack "\x56\x34\x12\x90\x78\x56\x34\x12") `shouldBe` Just 0x1234567890123456

    it "u64 \"\\x12\\x00\\x00\\x00\\x00\\x00\\x00\\x00\"" $ do
      u64 (BS.pack "\x12\x00\x00\x00\x00\x00\x00\x00") `shouldBe` Just 0x12

    it "u64 with worng length" $ do
      u64 (BS.replicate 10 'A') `shouldBe` Nothing

  describe "Pwn.Packing.u64be" $ do
    it "u64be \"\\x12\\x34\\x56\\x78\\x90\\x12\\x34\\x56\"" $ do
      u64be (BS.pack "\x12\x34\x56\x78\x90\x12\x34\x56") `shouldBe` Just 0x1234567890123456

    it "u64be \"\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x12\"" $ do
      u64be (BS.pack "\x00\x00\x00\x00\x00\x00\x00\x12") `shouldBe` Just 0x12

    it "u64be with worng length" $ do
      u64be (BS.replicate 10 'A') `shouldBe` Nothing
