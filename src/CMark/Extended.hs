{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module CMark.Extended
  ( module CMark,
    renderMarkdown,
    extractMetaDescription,
    linkifyHeaders,
    slugify,
  )
where

import CMark (Node (Node), NodeType (..), commonmarkToHtml, commonmarkToNode, optUnsafe)
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Protolude
import Text.Regex.PCRE.Heavy.Extended (gsub, myRe, sub)

slugify :: Text -> Text
slugify = T.intercalate "-" . T.words . T.toLower . T.map replaceInvalidChar
  where
    replaceInvalidChar c = if isAlphaNum c then c else ' '

linkifyHeaders :: Text -> Text
linkifyHeaders = gsub [myRe|^(#{2,}) (.*)$|] anchor
  where
    anchor (p : h : _) =
      p <> " <a class=\"h-anchor\" id=\"" <> h' <> "\" href=\"#" <> h' <> "\">#</a>" <> h
      where
        h' = slugify $ sub [myRe|^\[(.*)\]\(.*\)$|] title h
        title (t : _) = t
        title _ = h
    anchor _ = ""

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
