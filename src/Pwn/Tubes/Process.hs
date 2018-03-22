module Pwn.Tubes.Process
  ( Process (commandName, commandArgs, processID)
  , process
  ) where

import           Control.Monad            (void)
import           Data.Monoid              ((<>))
import           System.IO
import           System.Process
import           System.Process.Internals

import           Pwn.Log
import qualified Pwn.Tubes.Tube           as T

data Process = Process { commandName   :: FilePath
                       , commandArgs   :: [String]
                       , processID     :: Int
                       , stdinHandle   :: Handle
                       , stdoutHandle  :: Handle
                       , processHandle :: ProcessHandle
                       }

instance T.Tube Process where
  inputHandle  = stdinHandle
  outputHandle = stdoutHandle
  wait         = wait
  close        = close
  shutdown     = shutdown

-- https://stackoverflow.com/a/27388709
getPid :: ProcessHandle -> IO (Maybe Int)
getPid ph = withProcessHandle ph $ \ph_ ->
  return $ case ph_ of
                OpenHandle x   -> Just $ fromIntegral x
                _              -> Nothing

process :: FilePath -> [String] -> IO Process
process cmd args = do
  let logstr = "Starting process '" <> cmd <> "'"
  status logstr
  (Just ih, Just oh, _, ph) <-
    createProcess (proc cmd args) { std_in  = CreatePipe
                                  , std_out = CreatePipe
                                  }
  Just pid <- getPid ph
  success $ logstr <> ": Done (pid " <> show pid <> ")"
  mapM_ (`hSetBuffering` NoBuffering) [ ih, oh ]
  return $ Process cmd args pid ih oh ph

wait :: Process -> IO ()
wait p = void $ waitForProcess $ processHandle p

close :: Process -> IO ()
close = terminateProcess . processHandle

shutdown :: Process -> IO ()
shutdown = close
