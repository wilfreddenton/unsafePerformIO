module Lib.Effects.Random where

import           Control.Lens          (( # ))
import           Crypto.Random.Entropy (getEntropy)
import           Data.ByteArray        (ByteArray)
import           Lib.Effects.Logger    (MonadLogger)
import           Lib.Error             (CanApiError, wrapIO, _RngError)
import           Protolude

-- Implementations

-- IO

getRandomBytesIO :: (MonadLogger m, MonadIO m, CanApiError e m, ByteArray b) => Int -> m b
getRandomBytesIO n = wrapIO (_RngError #) $ getEntropy n
