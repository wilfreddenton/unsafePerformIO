module Data.Aeson.Extended (
  module Data.Aeson
, snakeNoPrefix
) where

import           Data.Aeson
import           Data.Aeson.Casing (snakeCase)
import           Data.Char         (isLower)
import           Data.String       (String)
import           Protolude

dropPrefix :: String -> String
dropPrefix = dropWhile isLower

snakeNoPrefix :: Options
snakeNoPrefix = defaultOptions { fieldLabelModifier = snakeCase . dropPrefix }
