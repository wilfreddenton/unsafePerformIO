module Lib.Effects.Time where

import           Data.Time (UTCTime, getCurrentTime)
import           Protolude

class Monad m => MonadTime m where
  now :: m UTCTime

nowIO :: MonadIO m => m UTCTime
nowIO = liftIO getCurrentTime
