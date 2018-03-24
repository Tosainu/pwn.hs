module Pwn.Tubes.Process
  ( Process (commandName, commandArgs, processID)
  , process
  ) where

import           Control.Monad            (void)
import           Control.Monad.IO.Class
import           Data.Monoid              ((<>))
import           System.IO
import           System.Process
import           System.Process.Internals

import           Pwn.Config
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
                OpenHandle x -> Just $ fromIntegral x
                _            -> Nothing

process :: MonadPwn m => FilePath -> [String] -> m Process
process cmd args = do
  let logstr = "Starting process '" <> cmd <> "'"
  status logstr
  (Just ih, Just oh, _, ph) <-
    liftIO $ createProcess (proc cmd args) { std_in  = CreatePipe
                                           , std_out = CreatePipe
                                           }
  Just pid <- liftIO $ getPid ph
  success $ logstr <> ": Done (pid " <> show pid <> ")"
  liftIO $ mapM_ (`hSetBuffering` NoBuffering) [ ih, oh ]
  return $ Process cmd args pid ih oh ph

wait :: MonadPwn m => Process -> m ()
wait p = liftIO $ void $ waitForProcess $ processHandle p

close :: MonadPwn m => Process -> m ()
close = liftIO . terminateProcess . processHandle

shutdown :: MonadPwn m => Process -> m ()
shutdown = close
