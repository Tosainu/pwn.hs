{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Pwn.Config
  ( Config (..)
  , Endian (..)
  , MonadPwn (..)
  , Pwn
  , runPwn
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

data Config = Config { arch   :: String
                     , bits   :: Int
                     , endian :: Endian
                     , os     :: String
                     } deriving (Eq, Show)

data Endian = Little | Big deriving (Eq, Show)

class MonadIO m => MonadPwn m where
  getConfig :: m Config
  getConfig = return $ Config { arch   = "amd64"
                              , bits   = 64
                              , endian = Little
                              , os     = "linux"
                              }

instance MonadPwn IO where

newtype Pwn a = Pwn (ReaderT Config IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadPwn Pwn where
  getConfig = Pwn ask

runPwn :: MonadIO m => Config -> Pwn a -> m a
runPwn config (Pwn m) = liftIO $ runReaderT m config
