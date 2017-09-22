{-# LANGUAGE RecordWildCards #-}

module Pwn.Tubes.Process
  ( Process (..)
  , process
  ) where

import           Control.Monad            (void)
import           Data.Monoid              ((<>))
import           System.IO
import           System.Process
import           System.Process.Internals

import           Pwn.Log
import qualified Pwn.Tubes.Tube           as T

data Process = Process { commmand :: FilePath
             , args               :: [String]
             , pid                :: Int
             , hstdin             :: Handle
             , hstdout            :: Handle
             , hproc              :: ProcessHandle
             }

instance T.Tube Process where
  inputHandle  = hstdin
  outputHandle = hstdout
  wait         = wait
  close        = close
  shutdown     = shutdown

-- https://stackoverflow.com/a/27388709
getPid :: ProcessHandle -> IO (Maybe Int)
getPid ph = withProcessHandle ph $ \ph_ ->
  return $ case ph_ of
                OpenHandle x   -> Just $ fromIntegral x
                ClosedHandle _ -> Nothing

process :: FilePath -> [String] -> IO Process
process commmand args = do
  let logstr = "Starting process '" <> commmand <> "'"
  status logstr
  (Just hstdin, Just hstdout, _, hproc)
      <- createProcess (proc commmand args) { std_in  = CreatePipe
                                            , std_out = CreatePipe
                                            }
  Just pid <- getPid hproc
  success $ logstr <> ": Done (pid " <> show pid <> ")"
  mapM_ (`hSetBuffering` NoBuffering) [ hstdin, hstdout ]
  return Process {..}

wait :: Process -> IO ()
wait p = void $ waitForProcess $ hproc p

close :: Process -> IO ()
close = terminateProcess . hproc

shutdown :: Process -> IO ()
shutdown = close
