{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App where

import           Control.Lens          ((^.))
import           Crypto.Random         (MonadRandom, getRandomBytes)
import           Crypto.Random.Entropy (getEntropy)
import           Katip                 (Katip, KatipContext, KatipContextT,
                                        runKatipContextT)
import           Lib.Effects.Logger    (MonadLogger (..), debugKatip,
                                        errorKatip, infoKatip, warnKatip,
                                        withContextKatip, withNamespaceKatip)
import           Lib.Effects.Post      (MonadPost, getPostBySlug,
                                        getPostBySlugPure, getPosts,
                                        getPostsPure)
import           Lib.Effects.Time      (MonadTime, now, nowIO)
import           Lib.Env               (AppEnv, HasLoggerEnv, loggerContext,
                                        loggerLogEnv, loggerNamespace)
import           Lib.Error             (AppError, toHttpError)
import           Network.Wai           (Request)
import           Protolude
import           Servant               (Handler)

newtype App a = App {
  unApp :: KatipContextT (ReaderT AppEnv (ExceptT AppError IO)) a
} deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO, Katip, KatipContext)

instance MonadLogger App where
  debug = debugKatip
  error = errorKatip
  info = infoKatip
  warn = warnKatip

  withNamespace = withNamespaceKatip
  withContext = withContextKatip

instance MonadPost App where
  getPosts = getPostsPure
  getPostBySlug = getPostBySlugPure

instance MonadTime App where
  now = nowIO

instance MonadRandom App where
  getRandomBytes = liftIO . getEntropy

runLoggerT :: HasLoggerEnv e => e -> KatipContextT m a -> m a
runLoggerT env = runKatipContextT (env^.loggerLogEnv) (env^.loggerContext) (env^.loggerNamespace)

appToHandler :: AppEnv -> Request -> App a -> Handler a
appToHandler env req app = do
  res <- liftIO . runExceptT . flip runReaderT env . runLoggerT env $ unApp app
  either (\e -> throwError (toHttpError req e)) pure res
