module Lib.Effects.Time where

import Control.Lens ((#))
import Data.Time (UTCTime, getCurrentTime)
import Lib.Effects.Logger (MonadLogger)
import Lib.Error (CanApiError, _GetTimeError, wrapIO)
import Protolude

class Monad m => MonadTime m where
  now :: m UTCTime

nowIO :: (MonadLogger m, MonadIO m, CanApiError e m) => m UTCTime
nowIO = wrapIO (_GetTimeError #) getCurrentTime
