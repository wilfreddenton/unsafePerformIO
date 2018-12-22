{-# LANGUAGE TemplateHaskell #-}

module Lib.Env where

import           Control.Lens (makeClassy)

data ServerEnv = ServerEnv { _serverPort :: Int }
makeClassy ''ServerEnv

data AppEnv = AppEnv { _appEnvServer :: ServerEnv }
makeClassy ''AppEnv

instance HasServerEnv AppEnv where
  serverEnv = appEnvServer . serverEnv
