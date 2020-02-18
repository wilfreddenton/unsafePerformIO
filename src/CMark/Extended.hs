{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module CMark.Extended
  ( module CMark,
    renderMarkdown,
    extractMetaDescription,
    linkifyHeaders,
    slugify,
    reifyFootnotes,
  )
where

import CMark (Node (Node), NodeType (..), commonmarkToHtml, commonmarkToNode, optUnsafe)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Protolude
import Text.Regex.PCRE.Heavy.Extended (gsub, ree, scan, sub)

slugify :: Text -> Text
slugify = T.intercalate "-" . T.words . T.toLower . T.map replaceInvalidChar
  where
    replaceInvalidChar c = if isAlphaNum c then c else ' '

linkifyHeaders :: Text -> Text
linkifyHeaders = gsub [ree|^(#{2,}) (.*)$|] anchor
  where
    anchor (p : h : _) =
      p <> " <a class=\"h-anchor\" id=\"" <> h' <> "\" href=\"#" <> h' <> "\">#</a>" <> h
      where
        h' = slugify $ sub [ree|^\[(.*)\]\(.*\)$|] title h
        title (t : _) = t
        title _ = h
    anchor _ = ""

reifyFootnotes :: Text -> Text
reifyFootnotes body =
  if length footnotesT > 0
    then
      T.strip (gsub footnoteR footnoteS $ gsub referenceR referenceS body)
        <> "\n\n---\n\n"
        <> footnotes
    else body
  where
    referenceR = [ree|\[\^([^\]]+)\](?!:)|]
    footnoteR = [ree|\n\n\[\^([^\]]+)\]:((.*)(\n+  .*)*)|]
    referencesM :: Map Text Int
    referencesM = Map.fromList $ zip (extractId <$> scan referenceR body) [1 ..]
      where
        extractId (_, [x]) = x
        extractId _ = ""
    footnotesT = fmap extractParts $ scan footnoteR body
      where
        extractParts (_, (r : b : _)) = (r, b)
        extractParts _ = ("", "")
    footnotes =
      "<ol>"
        <> ( T.unlines
               . fmap snd
               . filter ((/=) 0 . fst)
               . sortOn fst
               $ fmap reify footnotesT
           )
        <> "</ol>"
      where
        reify (r, b) =
          ( reference,
            "<li "
              <> id
              <> ">"
              <> renderMarkdown
                ( T.strip b
                    <> " <a "
                    <> "class=\"rfn\" "
                    <> href
                    <> ">↩</a>"
                )
              <> "</li>"
          )
          where
            reference = maybe 0 identity $ Map.lookup r referencesM
            id = "id=\"fnr-" <> show reference <> "\""
            href = "href=\"#fn-" <> show reference <> "\""
    referenceS :: [Text] -> Text
    referenceS [r] = case Map.lookup r referencesM of
      Just i ->
        "<sup><a "
          <> id i
          <> " "
          <> href i
          <> ">"
          <> show i
          <> "</a></sup>"
      Nothing -> ""
      where
        id i = "id=\"fn-" <> show i <> "\""
        href i = "href=\"#fnr-" <> show i <> "\""
    referenceS _ = ""
    footnoteS :: Text -> Text
    footnoteS _ = ""

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
