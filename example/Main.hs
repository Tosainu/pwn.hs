module Main where

import qualified Data.ByteString.Char8 as BS
import           Data.Monoid           ((<>))
import           Numeric               (showHex)

import           Pwn

main :: IO ()
main = do
  putStrLn "- Pwn.Log ----------"
  status  "status"
  success "success"
  failure "failure"
  debug   "debug"
  info    "info"
  warning "warning"

  putStrLn ""
  putStrLn "- Pwn.Packing ------"
  let n1 = 0x4142434445464748
      Just p11 = p64(n1)
      Just p12 = p64be(n1)
      Just u11 = u64(p11)
      Just u12 = u64be(p12)
  putStrLn $ "n1  = 0x" <> showHex n1 ""
  putStrLn $ "p11 = p64(n1)   = " <> BS.unpack p11
  putStrLn $ "p12 = p64be(n1) = " <> BS.unpack p12
  putStrLn $ "u11 = u64(p11)   = 0x" <> showHex u11 ""
  putStrLn $ "u12 = u64be(p12) = 0x" <> showHex u12 ""

  let n2 = 0x41424344
      Just p21 = p32(n2)
      Just p22 = p32be(n2)
      Just u21 = u32(p21)
      Just u22 = u32be(p22)
  putStrLn $ "n2  = 0x" <> showHex n2 ""
  putStrLn $ "p21 = p32(n2)   = " <> BS.unpack p21
  putStrLn $ "p22 = p32be(n2) = " <> BS.unpack p22
  putStrLn $ "u21 = u32(p21)   = 0x" <> showHex u21 ""
  putStrLn $ "u22 = u32be(p22) = 0x" <> showHex u22 ""
