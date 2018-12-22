{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App where

import           Control.Lens           ((^.))
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Katip                  (Katip, KatipContext, KatipContextT,
                                         runKatipContextT)
import           Lib.Effects.Logger     (MonadLogger, log, logKatip)
import           Lib.Env                (AppEnv, loggerContext, loggerLogEnv,
                                         loggerNamespace)
import           Lib.Error              (AppError, toHttpError)
import           Servant                (Handler)

newtype App a = App {
  unApp :: KatipContextT (ReaderT AppEnv (ExceptT AppError IO)) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO, Katip, KatipContext)

instance MonadLogger App where
  log = logKatip

appToHandler :: AppEnv -> App a -> Handler a
appToHandler env app = do
  res <- liftIO . runExceptT . flip runReaderT env . runLogger $ unApp app
  either (\e -> throwError (toHttpError e)) pure res
  where
    runLogger = runKatipContextT (env^.loggerLogEnv) (env^.loggerContext) (env^.loggerNamespace)
