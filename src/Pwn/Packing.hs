module Pwn.Packing
  ( Endian (..)
  , pack
  , p32
  , p32be
  , p64
  , p64be
  , unpack
  , u32
  , u32be
  , u64
  , u64be
  ) where

import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (chr, ord)
import           Data.Word             (Word32, Word64)

data Endian = Little | Big deriving Eq

nthByte :: (Bits a, Integral a) => (a, Int) -> Char
nthByte (x, n) = chr . fromIntegral $ shiftR (x .&. shiftL 0xff b) b
  where b = n * 8

toBytes :: Int -> Int
toBytes = flip quot 8

pack :: (Bits a, Integral a) => Int -> Endian -> a -> Maybe ByteString
pack size endian value
  | size < 8 || mod size 8 /= 0 = Nothing
  | endian == Little = Just $ pack' zipped
  | endian == Big    = Just $ pack' . reverse $ zipped
  | otherwise        = Nothing
  where bytes  = toBytes size
        zipped = zip (replicate size value) [0..bytes - 1]
        pack'  = BS.pack . map nthByte

p32 :: Word32 -> Maybe ByteString
p32 = pack 32 Little

p32be :: Word32 -> Maybe ByteString
p32be = pack 32 Big

p64 :: Word64 -> Maybe ByteString
p64 = pack 64 Little

p64be :: Word64 -> Maybe ByteString
p64be = pack 64 Big

unpack :: (Bits a, Integral a) => Int -> Endian -> ByteString -> Maybe a
unpack size endian str
  | size < 8 || mod size 8 /= 0 || BS.length str /= bytes = Nothing
  | endian == Little = Just $ unpack' $ zip ustr [0..bytes - 1]
  | endian == Big    = Just $ unpack' $ zip ustr $ reverse [0..bytes - 1]
  | otherwise        = Nothing
  where bytes = toBytes size
        ustr  = map (fromIntegral . ord) . BS.unpack $ str
        unpack' = sum . map (\(x, n) -> shiftL x $ n * 8)

u32 :: ByteString -> Maybe Word32
u32 = unpack 32 Little

u32be :: ByteString -> Maybe Word32
u32be = unpack 32 Big

u64 :: ByteString -> Maybe Word64
u64 = unpack 64 Little

u64be :: ByteString -> Maybe Word64
u64be = unpack 64 Big
