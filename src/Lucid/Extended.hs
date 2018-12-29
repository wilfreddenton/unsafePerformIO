module Lucid.Extended (
  module Lucid
, Template (..)
, container_
, row_
, colMd4_
, colMd8_
) where

import           Data.Aeson.Extended (ToJSON, toJSON)
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
      title_ . toHtml $ "unsafePerformIO | " <> title
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/github.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      script_ [src_ "/static/js/script.js"] ("" :: Text)
      script_ [src_ "/static/js/highlight.pack.js"] ("" :: Text)
      script_ "hljs.initHighlightingOnLoad()"
    body_ $ do
      container_ . row_ $ do
        colMd4_ . nav_ $ do
          h1_ [id_ "title"] $ a_ [href_ "/"] "unsafePerformIO"
          p_ "[ Author: Wilfred Denton ]"
          ul_ $ do
            li_ $ a_ [href_ "https://github.com/wilfreddenton/resume/blob/master/wilfred_denton_resume.pdf", target_ "_blank"] "Resumé"
            li_ $ a_ [href_ "/contact"] "Contact"
            li_ $ a_ [href_ "/pgp"] "PGP Key"
        colMd8_ . div_ [class_ "content"] $ toHtml a
      container_ [id_ "footer-container"] . footer_ $ do
        div_ $ span_ "BTC: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "ETH: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "XMR: xxxxxxxxxxxxxxxxxx"
        a_ [id_ "license", rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/3.0/us/"] $
          img_ [ alt_ "Creative Commons License"
              , style_ "border-width:0"
              , src_ "https://i.creativecommons.org/l/by-sa/3.0/us/80x15.png"
              ]
