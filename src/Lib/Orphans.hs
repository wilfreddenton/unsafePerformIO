{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Orphans where

import           Data.Aeson (Value)
import           Katip      (LogItem (..), PayloadSelection (AllKeys), ToObject)

instance ToObject Value where

instance LogItem Value where
  payloadKeys _verb _a = AllKeys
