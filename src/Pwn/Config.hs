module Pwn.Config
  ( Config (..)
  , Endian (..)
  ) where

data Config = Config { arch   :: String
                     , bits   :: Int
                     , endian :: Endian
                     , os     :: String
                     } deriving (Eq, Show)

data Endian = Little | Big deriving (Eq, Show)
