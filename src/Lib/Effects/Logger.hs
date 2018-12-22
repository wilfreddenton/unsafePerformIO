{-# LANGUAGE TemplateHaskell #-}

module Lib.Effects.Logger where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.Text.IO           as T
import           Katip                  (Katip, KatipContext, Severity (InfoS),
                                         logStr, logTM)

class Monad m => MonadLogger m where
  log :: Text -> m ()

logIO :: MonadIO m => Text -> m ()
logIO = liftIO . T.putStrLn

logKatip :: (Katip m, KatipContext m) => Text -> m ()
logKatip = $(logTM) InfoS . logStr
