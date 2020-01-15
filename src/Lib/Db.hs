{-# LANGUAGE ConstraintKinds #-}

module Lib.Db where

import Control.Lens ((#))
import Control.Lens (view)
import Database.SQLite.Simple (Connection)
import Lib.Effects.Logger (MonadLogger)
import Lib.Env (CanDbEnv, dConn)
import Lib.Error (CanDbError, _DbSqliteError, wrapIO)
import Protolude

type CanDb e r m = (CanDbError e m, CanDbEnv r m)

liftDbAction :: (MonadLogger m, MonadIO m, CanDb e r m) => IO a -> m a
liftDbAction = wrapIO (_DbSqliteError #)

onlyOneSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => (Connection -> IO [b]) -> m (Maybe b)
onlyOneSqlite action = do
  conn <- view dConn
  rows <- liftDbAction $ action conn
  pure $ case rows of
    [row] -> Just row
    _ -> Nothing
