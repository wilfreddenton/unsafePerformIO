{-# LANGUAGE TemplateHaskell #-}

module Lib.Env where

import           Control.Lens (makeClassy)
import           Katip        (ColorStrategy (ColorIfTerminal), LogContexts,
                               LogEnv, Namespace, Severity (DebugS),
                               Verbosity (V2), defaultScribeSettings,
                               initLogEnv, mkHandleScribe, registerScribe)
import           Protolude

data ServerEnv = ServerEnv { _serverPort :: Int }
makeClassy ''ServerEnv

data LoggerEnv = LoggerEnv {
  _loggerLogEnv    :: LogEnv
, _loggerContext   :: LogContexts
, _loggerNamespace :: Namespace
}
makeClassy ''LoggerEnv

newLoggerEnv :: IO LoggerEnv
newLoggerEnv = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout DebugS V2
  logEnv <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "development"
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
