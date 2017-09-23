module Pwn.Log
  ( status
  , success
  , failure
  , debug
  , info
  , warning
  ) where

import           System.Console.ANSI

type Symbol = (String, Color)

printSymbol :: Symbol -> IO ()
printSymbol (sym, color) = do
  putStr "["
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid color]
  putStr sym
  setSGR [Reset]
  putStr "] "

message :: Symbol -> String -> IO ()
message sym msg = printSymbol sym >> putStrLn msg

status :: String -> IO ()
status = message ("x", Magenta)

success :: String -> IO ()
success = message ("+", Green)

failure :: String -> IO ()
failure = message ("-", Red)

debug :: String -> IO ()
debug = message ("DEBUG", Red)

info :: String -> IO ()
info = message ("*", Blue)

warning :: String -> IO ()
warning = message ("!", Yellow)
