{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pwn.Config
  ( Config (..)
  , Endian (..)
  , MonadPwn (..)
  , Pwn (..)
  , defaultConfig
  , pwn
  , pwnWith
  ) where

import           Control.Monad.Reader

data Config = Config { arch   :: String
                     , bits   :: Int
                     , endian :: Endian
                     , os     :: String
                     } deriving (Eq, Show)

data Endian = Little | Big deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { arch   = "amd64"
  , bits   = 64
  , endian = Little
  , os     = "linux"
  }

class MonadIO m => MonadPwn m where
  getPwnConfig :: m Config
  getPwnConfig = return defaultConfig

newtype Pwn a = Pwn { runPwn :: ReaderT Config IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

instance MonadPwn Pwn where
  getPwnConfig = Pwn ask

pwn :: MonadIO m => Pwn a -> m a
pwn = pwnWith defaultConfig

pwnWith :: MonadIO m => Config -> Pwn a -> m a
pwnWith cfg = liftIO . flip runReaderT cfg . runPwn
