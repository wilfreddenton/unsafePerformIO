module Lucid.Extended (
  module Lucid
, Template (..)
) where

import           Data.Aeson (ToJSON, toJSON)
import           Lucid
import           Protolude

container_ :: Term arg result => arg -> result
container_ = termWith "div" [class_ " container "]

row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ " row "]

colMd4_ :: Term arg result => arg -> result
colMd4_ = termWith "div" [class_ " col-md-4 "]

colMd8_ :: Term arg result => arg -> result
colMd8_ = termWith "div" [class_ " col-md-8 "]

data Template a = Template Text a

instance ToJSON a => ToJSON (Template a) where
  toJSON (Template _ a) = toJSON a

instance ToHtml a => ToHtml (Template a) where
  toHtmlRaw = toHtml
  toHtml (Template title a) = doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/github.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      script_ [src_ "/static/js/script.js"] ("" :: Text)
      script_ [src_ "/static/js/highlight.pack.js"] ("" :: Text)
      script_ "hljs.initHighlightingOnLoad()"
    body_ . container_ . row_ $ do
      colMd4_ . nav_ $ do
        h1_ [id_ "title"] "unsafePerformIO"
        p_ "[ Author: Wilfred Denton ]"
        ul_ $ do
          li_ $ a_ [href_ "https://github.com/wilfreddenton/resume/blob/master/wilfred_denton_resume.pdf", target_ "_blank"] "Resumé"
          li_ $ a_ [href_ ""] "Contact"
          li_ $ a_ [href_ ""] "PGP Key"
      colMd8_ . div_ [class_ "content"] $ toHtml a
