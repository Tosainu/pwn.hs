{-# LANGUAGE RecordWildCards #-}

module Pwn.Tubes.Process
  ( Process (..)
  , process
  ) where

import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (($$))
import           Data.Conduit.Binary          (sinkHandle, sourceHandle)
import           Data.Monoid                  ((<>))
import           System.IO
import           System.Process
import           System.Process.Internals

import           Pwn.Log
import qualified Pwn.Tubes.Tube               as T

data Process = Process { commmand :: FilePath
             , args               :: [String]
             , pid                :: Int
             , hstdin             :: Handle
             , hstdout            :: Handle
             , hproc              :: ProcessHandle
             }

instance T.Tube Process where
  recv  = recv
  recvn = recvn
  send  = send
  wait  = wait
  close = close
  shutdown = shutdown
  interactive = interactive

-- https://stackoverflow.com/a/27388709
getPid :: ProcessHandle -> IO (Maybe Int)
getPid ph = withProcessHandle ph $ \ph_ ->
  return $ case ph_ of
                OpenHandle x   -> Just $ fromIntegral x
                ClosedHandle _ -> Nothing

process :: FilePath -> [String] -> IO Process
process commmand args = do
  let logstr = "Starting process '" <> commmand <> "'"
  liftIO $ status logstr
  (Just hstdin, Just hstdout, _, hproc)
      <- createProcess (proc commmand args) { std_in  = CreatePipe
                                            , std_out = CreatePipe
                                            }
  Just pid <- getPid hproc
  liftIO $ success $ logstr <> ": Done (pid " <> show pid <> ")"
  mapM_ (`hSetBuffering` NoBuffering) [ hstdin, hstdout ]
  return Process {..}

recv :: Process -> IO ByteString
recv p = BS.hGetSome (hstdout p) 4096

recvn :: Process -> Int -> IO ByteString
recvn p = BS.hGet (hstdout p)

send :: Process -> ByteString -> IO ()
send p = BS.hPut (hstdin p)

wait :: Process -> IO ()
wait p = void $ waitForProcess $ hproc p

close :: Process -> IO ()
close = terminateProcess . hproc

shutdown :: Process -> IO ()
shutdown = close

interactive :: Process -> IO ()
interactive p = do
  info "Entering interactive mode"
  let hi = hstdin p
      ho = hstdout p
  runResourceT $ do
    (rthread, _) <- allocate(forkIO $ runResourceT $ do
      sourceHandle ho $$ sinkHandle stdout
      liftIO $ info "Connection closed") killThread
    lift $ sourceHandle stdin $$ sinkHandle hi
    release rthread
  info "Leaving interactive mode"
