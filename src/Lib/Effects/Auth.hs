{-# LANGUAGE ConstraintKinds #-}

module Lib.Effects.Auth where

import           Control.Lens       (view, ( # ))
import           Crypto.Gpgme       (errorString, verifyDetached)
import qualified Data.Text.Encoding as T
import           Lib.Effects.Logger (MonadLogger)
import           Lib.Env            (CanAuthEnv, aCtx)
import           Lib.Error          (CanApiError, logAndThrow, wrapIO,
                                     _AuthorizationFailedError,
                                     _UnauthorizedError)
import           Protolude

-- Type

type CanAuth e a m = (CanApiError e m, CanAuthEnv a m)


-- Class

class Monad m => MonadAuth m where
  authorize :: Text -> Text -> m ()

-- Implementations

-- IO

authorizeIO :: (MonadLogger m, MonadIO m, CanAuth e a m) => Text -> Text -> m ()
authorizeIO sig clearText = do
  ctx <- view aCtx
  resultE <- wrapIO (_AuthorizationFailedError #) $ verifyDetached ctx (T.encodeUtf8 sig) (T.encodeUtf8 clearText)
  case resultE of
    Left _            -> fail
    Right [(e, _, _)] -> if errorString e == "Success" then pure () else fail
    Right _           -> fail
  where fail = logAndThrow (_UnauthorizedError # ())
