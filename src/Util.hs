{-# LANGUAGE FlexibleContexts #-}

module Util where

import           Control.Monad.Catch
import           Data.Typeable
import qualified System.Directory    as SD
import           System.Environment  (lookupEnv)
import           System.Exit         (ExitCode (..))
import           System.FilePath
import           System.Process      (readProcessWithExitCode)

----------------------------------------------------------------
-- Subprocess

newtype ProcessException = ProcessException (ExitCode, String) deriving (Show, Typeable)

instance Exception ProcessException

runProcess :: FilePath -> [String] -> IO (String, String)
runProcess cmd args = readProcessWithExitCode cmd args "" >>= handleProcessException

handleProcessException :: (MonadThrow m) => (ExitCode, String, String) -> m (String, String)
handleProcessException (ExitSuccess, out, err)  = return (out, err)
handleProcessException (excode, _, err)         = throwM $ ProcessException (excode, err)

----------------------------------------------------------------
-- Temporary Directory

withTemporaryDirectory :: String -> (FilePath -> IO a) -> IO a
withTemporaryDirectory prefix =
  bracket (createTemporaryDirectory prefix) SD.removeDirectoryRecursive

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = lookupEnv "XDG_RUNTIME_DIR" >>= maybe SD.getTemporaryDirectory return

createTemporaryDirectory :: String -> IO FilePath
createTemporaryDirectory prefix = do
  tempdir <- getTemporaryDirectory                    -- TODO: use prefix-pid
  SD.createDirectory (tempdir </> prefix) >> return (tempdir </> prefix)
