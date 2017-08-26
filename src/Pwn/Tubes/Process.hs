module Pwn.Tubes.Process
  ( Process (..)
  , process
  ) where

import           Control.Concurrent           (forkIO, killThread)
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (($$))
import           Data.Conduit.Binary          (sinkHandle, sourceHandle)
import           Data.Monoid                  ((<>))
import           System.IO
import           System.Posix.Types           (CPid)
import           System.Process
import           System.Process.Internals

import           Pwn.Log
import qualified Pwn.Tubes.Tube               as T

data Process = Process { commmand :: FilePath
             , args               :: [String]
             , pid                :: CPid
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
getPid :: ProcessHandle -> IO (Maybe CPid)
getPid ph = withProcessHandle ph $ \ph_ ->
  case ph_ of
        OpenHandle x   -> return $ Just x
        ClosedHandle _ -> return Nothing

process :: String -> [String] -> IO Process
process c a = do
  let logstr = "Starting process '" <> c <> "'"
  liftIO $ status logstr
  (Just hi, Just ho, _, ph)
    <- createProcess (proc c a){ std_in  = CreatePipe
                               , std_out = CreatePipe
                               }
  Just i <- getPid ph
  liftIO $ success $ logstr <> ": Done (pid " <> show i <> ")"
  mapM_ (`hSetBuffering` NoBuffering) [ hi, ho ]
  return $ Process c a i hi ho ph

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
