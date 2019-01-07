{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spec.Auth (
  authSpec
) where

import           Control.Lens       (makeClassy)
import           Lib.Effects.Auth   (MonadAuth, authorize, authorizeIO)
import           Lib.Effects.Logger (MonadLogger, debug, error, info, logPure,
                                     warn, withContext, withContextPure,
                                     withNamespace, withNamespacePure)
import           Lib.Env            (AuthEnv, HasAuthEnv, authEnv, newAuthEnv)
import           Lib.Error          (ApiError (UnauthorizedError),
                                     AppError (AppApiError))
import           Protolude
import           Test.Tasty.Hspec   (Spec, describe, it, shouldReturn)

data MockAppEnv = MockAppEnv {
  _mockAppAuthEnv :: AuthEnv
}
makeClassy ''MockAppEnv

instance HasAuthEnv MockAppEnv where
  authEnv = mockAppAuthEnv . authEnv

newtype MockApp a = MockApp {
  unMockApp :: ReaderT MockAppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader MockAppEnv, MonadError AppError, MonadIO)

instance MonadAuth MockApp where
  authorize = authorizeIO

instance MonadLogger MockApp where
  debug = logPure
  error = logPure
  info = logPure
  warn = logPure
  withNamespace = withNamespacePure
  withContext = withContextPure

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  env <- MockAppEnv <$> newAuthEnv ".gnupg-test" "wilfred.gpg"
  runExceptT . flip runReaderT env $ unMockApp action

authSpec :: Spec
authSpec = do
  describe "Auth" $
    it "does not authoriza invalid signature" $
      runMockApp (authorize "" "") `shouldReturn` Left (AppApiError UnauthorizedError)
