{-# LANGUAGE TemplateHaskell #-}

module Lib.Effects.Logger where

import           Data.Aeson.Extended (ToJSON, toJSON)
import           Katip               (KatipContext, Namespace (Namespace),
                                      Severity (..), katipAddContext,
                                      katipAddNamespace, logStr, logTM)
import           Lib.Orphans         ()
import           Protolude

-- Typeclass
class Monad m => MonadLogger m where
  debug :: Text -> m ()
  error :: Text -> m ()
  info :: Text -> m ()
  warn :: Text -> m ()

  withNamespace :: Text -> m a -> m a
  withContext :: ToJSON b => b -> m a -> m a

-- Implementations

-- Pure
logPure :: Monad m => Text -> m ()
logPure _ = pure ()

withNamespacePure :: Text -> m a -> m a
withNamespacePure _ action = action

withContextPure :: ToJSON b => b -> m a -> m a
withContextPure context action = let _ = toJSON context in action

-- Katip
logKatip :: KatipContext m => Severity -> Text -> m ()
logKatip severity = $(logTM) severity . logStr

debugKatip :: KatipContext m => Text -> m ()
debugKatip = logKatip DebugS

errorKatip :: KatipContext m => Text -> m ()
errorKatip = logKatip ErrorS

infoKatip :: KatipContext m => Text -> m ()
infoKatip = logKatip InfoS

warnKatip :: KatipContext m => Text -> m ()
warnKatip = logKatip WarningS

withNamespaceKatip :: KatipContext m => Text -> m a -> m a
withNamespaceKatip namespace = katipAddNamespace (Namespace [namespace])

withContextKatip :: (KatipContext m, ToJSON b) => b -> m a -> m a
withContextKatip context = katipAddContext (toJSON context)
