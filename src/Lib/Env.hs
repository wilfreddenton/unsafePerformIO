{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Env where

import           Control.Lens            (makeClassy)
import           Crypto.Gpgme            (Ctx, Protocol (OpenPGP), newCtx)
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
import           System.Directory        (doesDirectoryExist, doesFileExist)
import           System.Exit             (exitFailure)

data ServerEnv = ServerEnv {
  _sPort           :: Int
, _sSqliteDatabase :: FilePath
, _sGnuPgHomedir   :: FilePath
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
  _dConn :: Connection
}
makeClassy ''DbEnv

data AuthEnv = AuthEnv {
  _aCtx :: Ctx
}
makeClassy ''AuthEnv

customJsonFormatter :: LogItem a => ItemFormatter a
customJsonFormatter _color _verb Item{..} = fromText . toStrict . decodeUtf8 $ encode value
  where
    value = object [ "at" .= formatAsLogTime _itemTime
                   , "sev" .= renderSeverity _itemSeverity
                   , "data" .= toObject _itemPayload
                   , "msg" .= toLazyText (unLogStr _itemMessage)
                   , "ns" .= _itemNamespace
                   ]

validateFilePath :: MonadIO m => FilePath -> (FilePath -> IO Bool) -> m a -> m b -> m b
validateFilePath path validate failure success = do
  validB <- liftIO $ validate path
  case validB of
    False -> do
      _ <- failure
      liftIO exitFailure
    True -> success

newAuthEnv :: MonadIO m => FilePath -> m AuthEnv
newAuthEnv path = validateFilePath path doesDirectoryExist failure success
  where
    failure = putStrLn $ "no GnuPG homedir found at filepath: " <> path
    success = do
      ctx <- liftIO $ newCtx path "C" OpenPGP
      pure $ AuthEnv ctx

newDbEnv :: MonadIO m => FilePath -> m DbEnv
newDbEnv path = validateFilePath path doesFileExist failure success
  where
    failure = putStrLn $ "no sqlite database found at filepath: " <> path
    success = do
      conn <- liftIO $ open path
      pure $ DbEnv conn

newLoggerEnv :: MonadIO m => m LoggerEnv
newLoggerEnv = do
  handleScribe <- liftIO $ mkHandleScribeWithFormatter customJsonFormatter ColorIfTerminal stdout DebugS V3
  logEnv <- liftIO $ registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "IO" "development"
  pure $ LoggerEnv logEnv mempty mempty

data AppEnv = AppEnv {
  _appServerEnv :: ServerEnv
, _appLoggerEnv :: LoggerEnv
, _appDbEnv     :: DbEnv
, _appAuthEnv   :: AuthEnv
}
makeClassy ''AppEnv

instance HasServerEnv AppEnv where
  serverEnv = appServerEnv . serverEnv

instance HasLoggerEnv AppEnv where
  loggerEnv = appLoggerEnv . loggerEnv

instance HasDbEnv AppEnv where
  dbEnv = appDbEnv . dbEnv

instance HasAuthEnv AppEnv where
  authEnv = appAuthEnv . authEnv

type CanServerEnv a m = (MonadReader a m, HasServerEnv a)

type CanDbEnv a m = (MonadReader a m, HasDbEnv a)

type CanAuthEnv a m = (MonadReader a m, HasAuthEnv a)
