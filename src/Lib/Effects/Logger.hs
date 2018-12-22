module Lib.Effects.Logger where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.Text.IO           as T

class Monad m => MonadLogger m where
  log :: Text -> m ()

logIO :: MonadIO m => Text -> m ()
logIO = liftIO . T.putStrLn
