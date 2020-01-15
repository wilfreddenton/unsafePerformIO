module Lib.Effects.Random where

import Control.Lens ((#))
import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray (ByteArray)
import Lib.Effects.Logger (MonadLogger)
import Lib.Error (CanApiError, _RngError, wrapIO)
import Protolude

-- Implementations

-- IO

getRandomBytesIO :: (MonadLogger m, MonadIO m, CanApiError e m, ByteArray b) => Int -> m b
getRandomBytesIO = wrapIO (_RngError #) . getEntropy
