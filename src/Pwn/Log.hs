module Pwn.Log
  ( status
  , success
  , failure
  , debug
  , info
  , warning
  ) where

import           Data.Monoid ((<>))

data TermCode = Reset
              | Bold
              | Black
              | Red
              | Green
              | Yellow
              | Blue
              | Magenda
              | Cyan
              | White
              deriving Eq

instance Show TermCode where
  show Reset   = "\x1b[0m"
  show Bold    = "\x1b[1m"
  show Black   = "\x1b[30m"
  show Red     = "\x1b[31m"
  show Green   = "\x1b[32m"
  show Yellow  = "\x1b[33m"
  show Blue    = "\x1b[34m"
  show Magenda = "\x1b[35m"
  show Cyan    = "\x1b[36m"
  show White   = "\x1b[37m"

markup :: [TermCode] -> String -> String
markup tc str = concat $ tcstr ++ [str, show Reset]
  where tcstr = map show tc

message :: String -> String -> IO ()
message sym str = putStrLn $ "[" <> sym <> "] " <> str

status :: String -> IO ()
status = message $ markup [Magenda] "x"

success :: String -> IO ()
success = message $ markup [Bold, Green] "+"

failure :: String -> IO ()
failure = message $ markup [Bold, Red]  "-"

debug :: String -> IO ()
debug = message $ markup [Bold, Red] "DEBUG"

info :: String -> IO ()
info = message $ markup [Bold, Blue] "*"

warning :: String -> IO ()
warning = message $ markup [Bold, Yellow] "!"
