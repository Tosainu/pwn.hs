module Pwn.Tubes.Process
  ( Process (commandName, commandArgs, processID)
  , process
  ) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import qualified Data.Conduit.Binary    as C (sinkHandle, sourceHandle)
import           System.IO
import           System.Process

import           Pwn.Config
import           Pwn.Log
import qualified Pwn.Tubes.Tube         as T

data Process = Process { commandName   :: FilePath
                       , commandArgs   :: [String]
                       , processID     :: Maybe Pid
                       , stdinHandle   :: Handle
                       , stdoutHandle  :: Handle
                       , processHandle :: ProcessHandle
                       }

instance T.Tube Process where
  recv s    = liftIO $ BS.hGetSome (stdoutHandle s) 4096
  recvn s n = liftIO $ BS.hGet (stdoutHandle s) n
  send s    = liftIO . BS.hPut (stdinHandle s)
  isEOF c   = liftIO $ (||) <$> hIsEOF (stdinHandle c) <*> hIsEOF (stdoutHandle c)
  source    = C.sourceHandle . stdoutHandle
  sink      = C.sinkHandle . stdinHandle
  wait      = wait
  close     = close
  shutdown  = shutdown

process :: MonadPwn m => FilePath -> [String] -> m Process
process cmd args = do
  let logstr = "Starting process '" <> cmd <> "'"
  status logstr
  p <- liftIO $ startProcess cmd args
  success $ logstr <> ": Done (pid " <> (maybe "?" show (processID p)) <> ")"
  return p

startProcess :: FilePath -> [String] -> IO Process
startProcess cmd args =
    startProcess' =<< createProcess (proc cmd args) { std_in = CreatePipe , std_out = CreatePipe }
  where
    startProcess' (Just ih, Just oh, _, ph) = do
      pid <- getPid ph
      mapM_ (`hSetBuffering` NoBuffering) [ ih, oh ]
      return $ Process cmd args pid ih oh ph
    startProcess' _ = error "startProcess: createProcess failed"

wait :: MonadPwn m => Process -> m ()
wait p = liftIO $ void $ waitForProcess $ processHandle p

close :: MonadPwn m => Process -> m ()
close = liftIO . terminateProcess . processHandle

shutdown :: MonadPwn m => Process -> m ()
shutdown = close
