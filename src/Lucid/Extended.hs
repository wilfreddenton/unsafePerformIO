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
    extractMetaDescription,
  )
where

import CMark (Node (..), NodeType (..), commonmarkToHtml, commonmarkToNode, optUnsafe)
import qualified Data.Text as T
import Lucid hiding (button_, col_)
import Lucid.Base (makeAttribute)
import Protolude

renderMarkdown :: Text -> Text
renderMarkdown = commonmarkToHtml [optUnsafe]

nodeToText :: Node -> Text
nodeToText (Node _ nodeType children) = case nodeType of
  DOCUMENT -> T.strip foldNodes
  PARAGRAPH -> " " <> foldNodes
  SOFTBREAK -> " " <> foldNodes
  EMPH -> foldNodes
  STRONG -> foldNodes
  LINK _ _ -> foldNodes
  CODE t -> t
  TEXT t -> t
  _ -> ""
  where
    foldNodes = foldMap nodeToText children

extractMetaDescription :: Int -> Text -> Text
extractMetaDescription maxLen markdown =
  if T.length desc > maxLen
    then (<> "...") . T.strip . T.dropWhileEnd (/= ' ') $ T.take maxLen desc
    else desc
  where
    desc = nodeToText $ commonmarkToNode [] markdown

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
