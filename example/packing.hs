import qualified Data.ByteString.Char8 as BS
import           Data.Monoid           ((<>))
import           Numeric               (showHex)
import           Pwn

main :: IO ()
main = do
  info "examples of Pwn.Packing"
  putStrLn ""

  let n1 = 0x4142434445464748
      Just p11 = p64(n1)
      Just p12 = p64be(n1)
      Just u11 = u64(p11)
      Just u12 = u64be(p12)
  info    $ "n1  = 0x" <> showHex n1 ""
  success $ "p11 = p64(n1)   = " <> BS.unpack p11
  success $ "p12 = p64be(n1) = " <> BS.unpack p12
  success $ "u11 = u64(p11)   = 0x" <> showHex u11 ""
  success $ "u12 = u64be(p12) = 0x" <> showHex u12 ""

  putStrLn ""

  let n2 = 0x41424344
      Just p21 = p32(n2)
      Just p22 = p32be(n2)
      Just u21 = u32(p21)
      Just u22 = u32be(p22)
  info    $ "n2  = 0x" <> showHex n2 ""
  success $ "p21 = p32(n2)   = " <> BS.unpack p21
  success $ "p22 = p32be(n2) = " <> BS.unpack p22
  success $ "u21 = u32(p21)   = 0x" <> showHex u21 ""
  success $ "u22 = u32be(p22) = 0x" <> showHex u22 ""
