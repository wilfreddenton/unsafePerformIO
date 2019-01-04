{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Server.Auth where

import           Data.Aeson.Extended (FromJSON, ToJSON, genericParseJSON,
                                      genericToJSON, parseJSON, snakeNoPrefix,
                                      toJSON)
import           Protolude

data Signed a = Signed {
  sSignature :: Text
, sData      :: a
} deriving Generic

instance ToJSON a => ToJSON (Signed a) where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON a => FromJSON (Signed a) where
  parseJSON = genericParseJSON snakeNoPrefix
