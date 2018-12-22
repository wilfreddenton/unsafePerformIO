{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Lib.Effects.Logger     (MonadLogger, log, logIO)
import           Lib.Env                (AppEnv)
import           Lib.Error              (AppError, toHttpError)
import           Servant                (Handler)

newtype App a = App {
  unApp :: ReaderT AppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO)

instance MonadLogger App where
  log = logIO

appToHandler :: AppEnv -> App a -> Handler a
appToHandler env app = do
  res <- liftIO . runExceptT . flip runReaderT env $ unApp app
  either (\e -> throwError (toHttpError e)) pure res
