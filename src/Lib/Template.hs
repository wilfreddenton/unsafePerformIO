module Lib.Template where

import           Data.Aeson (ToJSON, toJSON)
import           Lucid      (ToHtml, body_, charset_, doctypehtml_, head_,
                             meta_, title_, toHtml, toHtmlRaw)
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
    body_ $ toHtml a
