module Lib.Env where

import           Data.Text (Text)

data AppEnv = AppEnv { appName :: Text }
