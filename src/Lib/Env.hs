{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Env where

import           Control.Lens            (makeClassy)
import           Data.Aeson.Extended     (ToJSON, encode, object, toJSON, (.=))
import           Data.Text.Lazy.Builder  (fromText, toLazyText)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Katip                   (ColorStrategy (ColorIfTerminal),
                                          Item (..), ItemFormatter, LogContexts,
                                          LogEnv, LogItem, Namespace,
                                          Severity (DebugS), Verbosity (V3),
                                          defaultScribeSettings, initLogEnv,
                                          mkHandleScribeWithFormatter,
                                          registerScribe, renderSeverity,
                                          toObject, unLogStr)
import           Katip.Format.Time       (formatAsLogTime)
import           Protolude               hiding (decodeUtf8)

data ServerEnv = ServerEnv { _serverPort :: Int }
makeClassy ''ServerEnv

instance ToJSON ServerEnv where
  toJSON (ServerEnv port) = object [ "port" .= port ]

data LoggerEnv = LoggerEnv {
  _loggerLogEnv    :: LogEnv
, _loggerContext   :: LogContexts
, _loggerNamespace :: Namespace
}
makeClassy ''LoggerEnv

customJsonFormatter :: LogItem a => ItemFormatter a
customJsonFormatter _color _verb Item{..} = fromText . toStrict . decodeUtf8 $ encode value
  where
    value = object [ "at" .= formatAsLogTime _itemTime
                   , "sev" .= renderSeverity _itemSeverity
                   , "data" .= toObject _itemPayload
                   , "msg" .= toLazyText (unLogStr _itemMessage)
                   , "ns" .= _itemNamespace
                   ]

newLoggerEnv :: IO LoggerEnv
newLoggerEnv = do
  handleScribe <- mkHandleScribeWithFormatter customJsonFormatter ColorIfTerminal stdout DebugS V3
  logEnv <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "HSP" "development"
  pure $ LoggerEnv logEnv mempty mempty

data AppEnv = AppEnv {
  _appServerEnv :: ServerEnv
, _appLoggerEnv :: LoggerEnv
}
makeClassy ''AppEnv

instance HasServerEnv AppEnv where
  serverEnv = appServerEnv . serverEnv

instance HasLoggerEnv AppEnv where
  loggerEnv = appLoggerEnv . loggerEnv
