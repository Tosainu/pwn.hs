{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pwn.Asm where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid           ((<>))
import           System.FilePath

import           Pwn.Config
import           Util

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
cpp Config { arch = a } src dst
  | a == "i386"  = void $ runProcess "cpp" ["-o", dst, src]
  | a == "amd64" = void $ runProcess "cpp" ["-o", dst, src]
  | otherwise    = fail "Unknown architecture"

as :: Config -> FilePath -> FilePath -> IO ()
as Config { arch = a } src dst
  | a == "i386"  = void $ runProcess "as" ["-32", "-o", dst, src]
  | a == "amd64" = void $ runProcess "as" ["-64", "-o", dst, src]
  | otherwise    = fail "Unknown architecture"

objcopy :: Config -> FilePath -> FilePath -> IO ()
objcopy Config { arch = a } src dst
  | a == "i386"  = void $ runProcess "objcopy" ["-j", ".text", "-O", "binary", src, dst]
  | a == "amd64" = void $ runProcess "objcopy" ["-j", ".text", "-O", "binary", src, dst]
  | otherwise    = fail "Unknown architecture"

objdump :: Config -> FilePath -> IO String
objdump Config { arch = a } src
  | a == "i386"  = objdump' ["-m", "i386", "-M", "intel,i386"]
  | a == "amd64" = objdump' ["-m", "i386", "-M", "intel,x86-64"]
  | otherwise    = fail "Unknown architecture"
  where
    objdump' opt = parseResult <$> runProcess "objdump" (opt <> ["-b", "binary" , "-D", src])
    parseResult  = unlines . drop 6 . lines . fst
