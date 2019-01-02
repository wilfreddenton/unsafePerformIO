{-# LANGUAGE ConstraintKinds #-}

module Lib.Db where

import           Control.Exception  (catch)
import           Control.Lens       (( # ))
import qualified Data.Text          as T
import           Lib.Effects.Logger (MonadLogger)
import           Lib.Env            (CanDbEnv)
import           Lib.Error          (CanDbError, logAndThrowError,
                                     _DbErrorSqlite)
import           Protolude

type CanDb e a m = (CanDbError e m, CanDbEnv a m)

liftDbAction :: (MonadLogger m, CanDbError e' m, MonadIO m) => IO a -> m a
liftDbAction action = do
  resultE <- liftIO $ catch (Right <$> action) (\e -> pure . Left . T.pack $ displayException (e :: SomeException))
  case resultE of
    Left err -> logAndThrowError $ _DbErrorSqlite # err
    Right a  -> pure a
