{-# LANGUAGE TemplateHaskell #-}

module Lib.Effects.Logger where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Katip                  (Katip, KatipContext, Severity (InfoS),
                                         logStr, logTM)
import           Protolude

class Monad m => MonadLogger m where
  log :: Text -> m ()

logPure :: Monad m => Text -> m ()
logPure _ = pure ()

logIO :: MonadIO m => Text -> m ()
logIO = liftIO . putText

logKatip :: (Katip m, KatipContext m) => Text -> m ()
logKatip = $(logTM) InfoS . logStr
