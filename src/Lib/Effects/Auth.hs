{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib.Effects.Auth where

import Control.Lens ((#), view)
import Crypto.Gpgme (errorString, verifyDetached)
import Data.Aeson.Extended
  ( FromJSON,
    ToJSON,
    genericParseJSON,
    genericToJSON,
    parseJSON,
    snakeNoPrefix,
    toJSON,
  )
import qualified Data.Text.Encoding as T
import Lib.Effects.Logger (MonadLogger)
import Lib.Env (CanAuthEnv, aCtx)
import Lib.Error
  ( CanApiError,
    _AuthorizationFailedError,
    _UnauthorizedError,
    logAndThrow,
    wrapIO,
  )
import Protolude

-- Type

data Signed a
  = Signed
      { sSignature :: Text,
        sData :: a
      }
  deriving (Generic)

instance ToJSON a => ToJSON (Signed a) where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON a => FromJSON (Signed a) where
  parseJSON = genericParseJSON snakeNoPrefix

type CanAuth e a m = (CanApiError e m, CanAuthEnv a m)

-- Class

class Monad m => MonadAuth m where
  authorize :: Text -> Text -> m ()

-- Implementations

-- IO

authorizeIO :: (MonadLogger m, MonadIO m, CanAuth e a m) => Text -> Text -> m ()
authorizeIO sig clearText = do
  ctx <- view aCtx
  resultE <- wrapIO (_AuthorizationFailedError #) . verifyDetached ctx (T.encodeUtf8 sig) $ T.encodeUtf8 clearText
  case resultE of
    Right [(e, _, _)] -> if errorString e == "Success" then pure () else fail
    _ -> fail
  where
    fail = logAndThrow (_UnauthorizedError # ())

-- Pure

authorizePure :: Monad m => Text -> Text -> m ()
authorizePure _ _ = pure ()
