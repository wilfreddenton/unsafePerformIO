{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Spec.Auth (
  authSpec
) where

import           Control.Lens                   (makeClassy, view, ( # ))
import           Crypto.Gpgme                   (Ctx, Protocol (OpenPGP),
                                                 SignMode (Detach), newCtx,
                                                 setArmor,
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
                                                 CanApiError, logAndThrow,
                                                 _AuthorizationFailedError)
import           Protolude
import           Servant                        (NoContent (NoContent))
import           Test.Hspec.Core.QuickCheck     (modifyMaxSuccess)
import           Test.QuickCheck                (property)
import           Test.QuickCheck.Instances.Text ()
import           Test.Tasty.Hspec               (Spec, describe, it, runIO,
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

aliceHomedir :: FilePath
aliceHomedir = ".gnupg-alice"

bobHomedir :: FilePath
bobHomedir = ".gnupg-bob"

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  env <- MockAppEnv <$> newAuthEnv aliceHomedir "wilfred.gpg"
  runExceptT . flip runReaderT env $ unMockApp action

makeSignature :: (MonadLogger m, CanApiError e m, MonadIO m) => Ctx -> Text -> m Text
makeSignature ctx clearText = do
  let clearTextBs = T.encodeUtf8 clearText
  liftIO $ setArmor True ctx
  liftIO $ setPassphraseCallback ctx (Just $ \_ _ _ -> pure $ Just "password")
  result <- liftIO $ sign ctx [] Detach clearTextBs
  case result of
    Right sig -> pure $ T.decodeUtf8 sig
    _         -> logAndThrow $ _AuthorizationFailedError # ""

authSpec :: Spec
authSpec = do
  let n = 20
      failure = Left $ AppApiError UnauthorizedError
  describe "Auth" $ do
    it "does not authorize random signatures and cleartext" . property $ \sig clearText -> do
      runMockApp (authorize sig clearText) `shouldReturn` failure

    bobCtx <- runIO $ newCtx bobHomedir "C" OpenPGP
    modifyMaxSuccess (const n) . it "does not authorize a invalid signature" . property $ \clearText -> do
      let action = do
            sig <- makeSignature bobCtx clearText
            authorize sig clearText
      runMockApp action `shouldReturn` failure

    modifyMaxSuccess (const n) . it "does authorize valid signatures" . property $ \clearText -> do
      let action = do
            ctx <- view aCtx
            sig <- makeSignature ctx clearText
            authorize sig clearText
            pure NoContent
      runMockApp action `shouldReturn` Right NoContent
