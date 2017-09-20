module Pwn.Tubes.ProcessSpec
  ( main
  , spec
  ) where

import           Control.Exception     (bracket)
import qualified Data.ByteString.Char8 as BS
import           Pwn.Tubes
import           System.IO.Error       (isEOFError)
import           Test.Hspec

main :: IO ()
main = hspec spec

withEcho :: (Process -> IO ()) -> IO ()
withEcho = bracket (process "echo" [teststr]) wait

withCat :: (Process -> IO ()) -> IO ()
withCat = bracket (process "cat" []) close

teststr :: String
teststr = teststr1 ++ "\n" ++ teststr2

teststr1 :: String
teststr1 = "abcdefg"

teststr2 :: String
teststr2 = "hijklmn"

spec :: Spec
spec = do
  around withEcho $ describe "Pwn.Tubes.Process.recv*" $ do
    it "recv" $ \proc -> do
      let s = BS.pack $ teststr ++ "\n"
      r <- recv proc
      r `shouldBe` s

    it "recvn" $ \proc -> do
      let s1 = BS.pack $ take 3 teststr
          s2 = BS.pack $ take 3 $ drop 3 teststr
      r1 <- recvn proc 3
      r1 `shouldBe` s1

      r2 <- recvn proc 3
      r2 `shouldBe` s2

    it "recvn EOFerror" $ \proc -> do
      recvn proc 999 `shouldThrow` isEOFError

    it "recvline" $ \proc -> do
      let s = BS.pack $ teststr1 ++ "\n"
      r <- recvline proc
      r `shouldBe` s

    it "recvuntil" $ \proc -> do
      let s = BS.pack $ drop 3 teststr1
      r <- recvuntil proc s
      r `shouldBe` BS.pack teststr1

  around withCat $ describe "Pwn.Tubes.Process.send*" $ do
    it "send" $ \proc -> do
      let s = BS.pack teststr
      send proc s
      r <- recv proc
      r `shouldBe` s

    it "sendline" $ \proc -> do
      let s1 = BS.pack $ teststr1
          s2 = BS.pack $ teststr1 ++ "\n"
      sendline proc s1
      r <- recv proc
      r `shouldBe` s2
