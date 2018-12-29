{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Orphans where

import           Data.Aeson.Extended (Value (..), object, (.=))
import           Katip               (LogItem (..), PayloadSelection (AllKeys),
                                      ToObject, toObject)
import           Protolude

instance ToObject Value where
  toObject v = case v of
    Object o     -> o
    a@(Array _)  -> makeObject "array" a
    s@(String _) -> makeObject "string" s
    n@(Number _) -> makeObject "number" n
    b@(Bool _)   -> makeObject "bool" b
    Null         -> makeObject "null" Null
    where makeObject k v' = toObject $ object [ k .= v' ]

instance LogItem Value where
  payloadKeys _verb _a = AllKeys
