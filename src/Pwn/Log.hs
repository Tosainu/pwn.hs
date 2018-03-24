module Pwn.Log
  ( status
  , success
  , failure
  , debug
  , info
  , warning
  ) where

import           Control.Monad.IO.Class
import           System.Console.ANSI

import           Pwn.Config

type Symbol = (String, Color)

printSymbol :: Symbol -> IO ()
printSymbol (sym, color) = do
  putStr "["
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid color]
  putStr sym
  setSGR [Reset]
  putStr "] "

message :: MonadPwn m => Symbol -> String -> m ()
message sym msg = liftIO $ printSymbol sym >> putStrLn msg

status :: MonadPwn m => String -> m ()
status = message ("x", Magenta)

success :: MonadPwn m => String -> m ()
success = message ("+", Green)

failure :: MonadPwn m => String -> m ()
failure = message ("-", Red)

debug :: MonadPwn m => String -> m ()
debug = message ("DEBUG", Red)

info :: MonadPwn m => String -> m ()
info = message ("*", Blue)

warning :: MonadPwn m => String -> m ()
warning = message ("!", Yellow)
