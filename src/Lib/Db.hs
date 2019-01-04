{-# LANGUAGE ConstraintKinds #-}

module Lib.Db where

import           Control.Lens       (( # ))
import           Lib.Effects.Logger (MonadLogger)
import           Lib.Env            (CanDbEnv)
import           Lib.Error          (CanDbError, wrapIO, _DbSqliteError)
import           Protolude

type CanDb e r m = (CanDbError e m, CanDbEnv r m)

liftDbAction :: (MonadLogger m, MonadIO m, CanDb e r m) => IO a -> m a
liftDbAction = wrapIO (_DbSqliteError #)
