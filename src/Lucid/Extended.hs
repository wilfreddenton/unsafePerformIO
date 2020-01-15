module Lucid.Extended
  ( module Lucid,
    property_,
    button_,
    container_,
    row_,
    col_,
    colSm4_,
    colSm6_,
    colSm8_,
    colMd4_,
    colMd8_,
    renderMarkdown,
  )
where

import Lucid hiding (button_, col_)
import Lucid.Base (makeAttribute)
import Protolude
import qualified Text.MMark as MMark

renderMarkdown :: Text -> Text -> Html ()
renderMarkdown name textToRender = case MMark.parse (show name) textToRender of
  Left _ -> p_ "invalid markdown" -- should never run
  Right m -> MMark.render m

property_ :: Text -> Attribute
property_ = makeAttribute "property"

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

colSm6_ :: Term arg result => arg -> result
colSm6_ = termWith "div" [class_ " col-sm-6 "]

colSm8_ :: Term arg result => arg -> result
colSm8_ = termWith "div" [class_ " col-sm-8 "]

colMd4_ :: Term arg result => arg -> result
colMd4_ = termWith "div" [class_ " col-md-4 "]

colMd8_ :: Term arg result => arg -> result
colMd8_ = termWith "div" [class_ " col-md-8 "]
