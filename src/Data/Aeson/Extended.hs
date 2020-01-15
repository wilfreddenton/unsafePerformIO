module Data.Aeson.Extended
  ( module Data.Aeson,
    snakeNoPrefix,
  )
where

import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import Data.Char (isAlphaNum, isLower)
import Data.String (String)
import Protolude

dropPrefix :: String -> String
dropPrefix = dropWhile f
  where
    f = (||) <$> isLower <*> (not . isAlphaNum)

snakeNoPrefix :: Options
snakeNoPrefix = defaultOptions {fieldLabelModifier = snakeCase . dropPrefix}
