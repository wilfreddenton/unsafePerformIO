module Lucid.Extended (
  module Lucid
, Template (..)
) where

import           Data.Aeson (ToJSON, toJSON)
import           Lucid
import           Protolude

data Template a = Template Text a

instance ToJSON a => ToJSON (Template a) where
  toJSON (Template _ a) = toJSON a

instance ToHtml a => ToHtml (Template a) where
  toHtmlRaw = toHtml
  toHtml (Template title a) = doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/style.css"]
      script_ [src_ "/static/script.js"] ("" :: Text)
    body_ $ toHtml a
