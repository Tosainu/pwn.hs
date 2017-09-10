{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pwn.Asm
  ( asm
  , disasm
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as BS
import           Data.Typeable
import           System.Directory       hiding (getTemporaryDirectory)
import           System.Environment     (lookupEnv)
import           System.Exit            (ExitCode (..))
import           System.FilePath        ((</>))
import           System.Process         (readProcessWithExitCode)

import           Pwn.Config

asm :: (MonadReader Config m, MonadIO m) => BS.ByteString -> m (Either String BS.ByteString)
asm src = do
  config <- ask
  src'   <- addAsmHeader src
  liftIO $ withTemporaryDirectory "pwn" (\path -> do
      let step1 = path </> "asm.S"
          step2 = path </> "cppasm.S"
          step3 = path </> "out.o"
          step4 = path </> "out.bin"
      BS.writeFile step1 src'
      cpp config step1 step2
      as config step2 step3
      objcopy config step3 step4
      Right <$> BS.readFile step4
    ) `catch` (\(e::SomeException) -> return $ Left $ show e)

disasm :: (MonadReader Config m, MonadIO m) => BS.ByteString -> m (Either String BS.ByteString)
disasm src = do
  config <- ask
  liftIO $ withTemporaryDirectory "pwn" (\path -> do
      let step1 = path </> "src.bin"
      BS.writeFile step1 src
      (Right . BS.pack) <$> objdump config step1
    ) `catch` (\(e::SomeException) -> return $ Left $ show e)

addAsmHeader :: (MonadReader Config m, MonadIO m) => BS.ByteString -> m BS.ByteString
addAsmHeader src = addAsmHeader' <$> ask <*> return src
  where
    addAsmHeader' Config { arch = "i386" }  = BS.append ".intel_syntax noprefix\n"
    addAsmHeader' Config { arch = "amd64" } = BS.append ".intel_syntax noprefix\n"
    addAsmHeader' _                         = id

cpp :: Config -> FilePath -> FilePath -> IO ()
cpp _ src dst = void $ runProcess "cpp" ["-o", dst, src]

as :: Config -> FilePath -> FilePath -> IO ()
as _ src dst = void $ runProcess "as" ["--64", "-o", dst, src]

objcopy :: Config -> FilePath -> FilePath -> IO ()
objcopy _ src dst = void $ runProcess "objcopy" ["-j", ".text", "-O", "binary", src, dst]

objdump :: Config -> FilePath -> IO String
objdump _ src = (stripHeader . fst) <$> runProcess "objdump"
                               [ "-b", "binary" , "-m", "i386" , "-M", "intel,x86-64" , "-D", src ]
  where
    stripHeader = unlines . drop 6 . lines

newtype ProcessException = ProcessException (ExitCode, String) deriving (Show, Typeable)

instance Exception ProcessException

runProcess :: FilePath -> [String] -> IO (String, String)
runProcess cmd args = readProcessWithExitCode cmd args "" >>= handleProcessException

handleProcessException :: (MonadThrow m) => (ExitCode, String, String) -> m (String, String)
handleProcessException (ExitSuccess, out, err)  = return (out, err)
handleProcessException (excode, _, err)         = throwM $ ProcessException (excode, err)

withTemporaryDirectory :: String -> (FilePath -> IO a) -> IO a
withTemporaryDirectory prefix = bracket (createTemporaryDirectory prefix) removeDirectoryRecursive

getTemporaryDirectory :: IO (Maybe FilePath)
getTemporaryDirectory = lookupEnv "XDG_RUNTIME_DIR"   -- TODO: suppport other directories

createTemporaryDirectory :: String -> IO FilePath
createTemporaryDirectory prefix = do
  tempdir <- getTemporaryDirectory                    -- TODO: use prefix-pid
  case tempdir of
       Just path  -> createDirectory (path </> prefix) >> return (path </> prefix)
       Nothing    -> fail "Could not get temporary directory"
