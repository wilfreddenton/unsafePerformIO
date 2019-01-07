{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spec.Auth (
  authSpec
) where

import           Control.Lens                   (makeClassy, view, ( # ))
import           Crypto.Gpgme                   (SignMode (Detach), setArmor,
                                                 setPassphraseCallback, sign)
import qualified Data.Text.Encoding             as T
import           Lib.Effects.Auth               (MonadAuth, authorize,
                                                 authorizeIO)
import           Lib.Effects.Logger             (MonadLogger, debug, error,
                                                 info, logPure, warn,
                                                 withContext, withContextPure,
                                                 withNamespace,
                                                 withNamespacePure)
import           Lib.Env                        (AuthEnv, HasAuthEnv, aCtx,
                                                 authEnv, newAuthEnv)
import           Lib.Error                      (ApiError (UnauthorizedError),
                                                 AppError (AppApiError),
                                                 logAndThrow,
                                                 _AuthorizationFailedError)
import           Protolude
import           Servant                        (NoContent (NoContent))
import           Test.Hspec.Core.QuickCheck     (modifyMaxSuccess)
import           Test.QuickCheck                (property)
import           Test.QuickCheck.Instances.Text ()
import           Test.Tasty.Hspec               (Spec, describe, it,
                                                 shouldReturn)

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
  describe "Auth" $ do
    it "does not authorize a invalid signature" $
      runMockApp (authorize "" "") `shouldReturn` Left (AppApiError UnauthorizedError)
    modifyMaxSuccess (const 20) . it "blah" . property $ \clearText -> do
      let clearTextBs = T.encodeUtf8 clearText
          action = do
            ctx <- view aCtx
            liftIO $ setArmor True ctx
            liftIO $ setPassphraseCallback ctx (Just $ \_ _ _ -> pure $ Just "password")
            result <- liftIO $ sign ctx [] Detach clearTextBs
            detachedSignature <- case result of
              Right sig -> pure sig
              _         -> logAndThrow $ _AuthorizationFailedError # ""
            authorize (T.decodeUtf8 detachedSignature) clearText
            pure NoContent
      runMockApp action `shouldReturn` Right NoContent
