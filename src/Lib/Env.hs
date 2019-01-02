{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Env where

import           Control.Lens            (makeClassy)
import           Data.Aeson.Extended     (ToJSON, encode, genericToJSON, object,
                                          snakeNoPrefix, toJSON, (.=))
import           Data.Text.Lazy.Builder  (fromText, toLazyText)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Database.SQLite.Simple  (Connection, open)
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
import           System.Directory        (doesFileExist)
import           System.Exit             (exitFailure)

data ServerEnv = ServerEnv {
  _sPort           :: Int
, _sSqliteDatabase :: FilePath
} deriving Generic
makeClassy ''ServerEnv

instance ToJSON ServerEnv where
  toJSON = genericToJSON snakeNoPrefix

data LoggerEnv = LoggerEnv {
  _lLogEnv    :: LogEnv
, _lContext   :: LogContexts
, _lNamespace :: Namespace
}
makeClassy ''LoggerEnv

data DbEnv = DbEnv {
  _dbConn :: Connection
}
makeClassy ''DbEnv

customJsonFormatter :: LogItem a => ItemFormatter a
customJsonFormatter _color _verb Item{..} = fromText . toStrict . decodeUtf8 $ encode value
  where
    value = object [ "at" .= formatAsLogTime _itemTime
                   , "sev" .= renderSeverity _itemSeverity
                   , "data" .= toObject _itemPayload
                   , "msg" .= toLazyText (unLogStr _itemMessage)
                   , "ns" .= _itemNamespace
                   ]

newLoggerEnv :: MonadIO m => m LoggerEnv
newLoggerEnv = do
  handleScribe <- liftIO $ mkHandleScribeWithFormatter customJsonFormatter ColorIfTerminal stdout DebugS V3
  logEnv <- liftIO $ registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "HSP" "development"
  pure $ LoggerEnv logEnv mempty mempty

newDbEnv :: MonadIO m => FilePath -> m DbEnv
newDbEnv path = do
  doesExist <- liftIO $ doesFileExist path
  case doesExist of
    False -> do
      putStrLn $ "no sqlite database found at filepath: " <> path
      liftIO exitFailure
    True -> do
      conn <- liftIO $ open path
      pure $ DbEnv conn

data AppEnv = AppEnv {
  _appServerEnv :: ServerEnv
, _appLoggerEnv :: LoggerEnv
, _appDbEnv     :: DbEnv
}
makeClassy ''AppEnv

instance HasServerEnv AppEnv where
  serverEnv = appServerEnv . serverEnv

instance HasLoggerEnv AppEnv where
  loggerEnv = appLoggerEnv . loggerEnv

instance HasDbEnv AppEnv where
  dbEnv = appDbEnv . dbEnv

type CanDb a m = (MonadReader a m, HasDbEnv a)
