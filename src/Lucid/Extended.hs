module Lucid.Extended (
  module Lucid
, Template (..)
, AuthorTemplate (..)
, container_
, row_
, col_
, colSm4_
, colSm8_
, colMd4_
, colMd8_
, renderMarkdown
) where

import           Data.Aeson.Extended (ToJSON, toJSON)
import           Lucid               hiding (button_, col_)
import           Protolude
import qualified Text.MMark          as MMark

renderMarkdown :: Text -> Text -> Html ()
renderMarkdown name textToRender = case MMark.parse (show name) textToRender of
  Left _  -> p_ "invalid markdown" -- should never run
  Right m -> MMark.render m

button_ :: Term arg result => arg -> result
button_ = termWith "a" [class_ "button"]

container_ :: Term arg result => arg -> result
container_ = termWith "div" [class_ " container "]

row_ :: Term arg result => arg -> result
row_ = termWith "div" [class_ " row "]

col_ :: Term arg result => arg -> result
col_ = termWith "div" [class_ " col "]

colSm4_ :: Term arg result => arg -> result
colSm4_ = termWith "div" [class_ " col-sm-4 "]

colSm8_ :: Term arg result => arg -> result
colSm8_ = termWith "div" [class_ " col-sm-8 "]

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
          h1_ [id_ "title"] $ button_ [href_ "/"] "unsafePerformIO"
          p_ "[ Author: Wilfred Denton ]"
          ul_ $ do
            li_ $ button_ [href_ "/about"] "About"
            li_ $ button_ [href_ "/contact"] "Contact"
            li_ $ button_ [href_ "https://github.com/wilfreddenton/resume/blob/master/wilfred_denton_resume.pdf", target_ "_blank"] "Resumé"
            li_ $ button_ [href_ "/pgp"] "PGP Key"
        colMd8_ . div_ [class_ "content"] $ toHtml a
      container_ [id_ "footer-container"] . footer_ $ do
        div_ $ span_ "BTC: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "ETH: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "XMR: xxxxxxxxxxxxxxxxxx"
        div_ [style_ "margin-top: 1rem"]$ do
          a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/3.0/us/"] $
            img_ [ alt_ "Creative Commons License"
                 , style_ "border-width:0"
                 , src_ "https://i.creativecommons.org/l/by-sa/3.0/us/80x15.png"
                 ]
          button_ [id_ "view-source", href_ "", (target_) "_blank"] "</>"

data AuthorTemplate = AuthorTemplate

instance ToHtml AuthorTemplate where
  toHtmlRaw = toHtml
  toHtml AuthorTemplate = doctypehtml_ $ do
    head_ $ do
      title_ "unsafePerformIO | author"
      meta_ [charset_ "utf-8"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      script_ [src_ "/static/js/openpgp.min.js"] ("" :: Text)
      script_ [src_ "/static/js/author.js"] ("" :: Text)
    body_ $ do
      container_ . row_ $ do
        col_ $ do
          h3_ "PGP Private Key"
          textarea_ [id_ "private-key", type_ "text", name_ "private-key", placeholder_ "PGP Private Key"] ""
          button_ [id_ "submit", href_ ""] "submit"
