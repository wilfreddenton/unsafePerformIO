{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.Effects.Post where

import           Data.Aeson.Extended (ToJSON, genericToJSON, snakeNoPrefix,
                                      toJSON)
import           Data.Char           (isAlphaNum)
import           Data.Map.Strict     as Map
import           Data.Map.Strict     (Map)
import qualified Data.Text           as T
import           Data.Time           (UTCTime (UTCTime), defaultTimeLocale,
                                      formatTime, fromGregorian,
                                      secondsToDiffTime)
import           Lib.Orphans         ()
import           Lucid.Extended      (HtmlT, ToHtml, class_, div_, h1_, h3_,
                                      href_, li_, renderMarkdown, span_,
                                      termWith, toHtml, toHtmlRaw, ul_)
import           Protolude

-- Type
data Post = Post {
  pSlug      :: Text
, pTitle     :: Text
, pCreatedAt :: UTCTime
, pBody      :: Text
} deriving (Eq, Show, Generic)

instance ToJSON Post where
  toJSON = genericToJSON snakeNoPrefix

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post {..} = div_ [class_ "post"] $ do
    h1_ $ toHtml pTitle
    toHtml $ renderMarkdown pTitle pBody

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem :: Monad m => Post -> HtmlT m ()
      asListItem Post {..} = li_ $ do
        termWith "a" [class_ "post-link", href_ $ "/posts/" <> pSlug] $ do
          h3_ $ do
            span_ [class_ "inset"] "_"
            toHtml $ pTitle
          span_ [class_ "post-list-date"] . toHtml . formatTime defaultTimeLocale "%b %d, %_Y" $ pCreatedAt

-- Typeclass
class Monad m => MonadPost m where
  getPosts :: m [Post]
  getPostBySlug :: Text -> m (Maybe Post)

-- Implementations

-- Pure
postsMap :: Map Text Post
postsMap = Map.fromList $ fmap makePost postData
  where
    makeSlug title createdAt =
      let createdAtStr = formatTime defaultTimeLocale "%_Y-%m-%d" createdAt
          modifiedTitle = T.intercalate "-" . T.words . T.toLower $ T.map (\c -> if isAlphaNum c then c else ' ') title
      in T.pack createdAtStr <> "-" <> modifiedTitle
    makePost (title, createdAt, body) =
      let slug = makeSlug title createdAt
      in (slug, Post slug title createdAt body)
    postData = [
        ("HTML Templating with Lucid", (UTCTime (fromGregorian 2018 12 30) (secondsToDiffTime 0)), "blah blah blah!")
      , ("Haskell JSON Tricks", (UTCTime (fromGregorian 2018 11 20) (secondsToDiffTime 0)), "blah blah blah!")
      , ("Haskell is Frustratingly Good", (UTCTime (fromGregorian 2017 2 12) (secondsToDiffTime 0)), "*unsafePerformIO*")
      , ("Foo", (UTCTime (fromGregorian 2008 8 22) (secondsToDiffTime 0)), "Let's go to the *Bar* ok?")
      , ("Hello, World!", (UTCTime (fromGregorian 1994 1 31) (secondsToDiffTime 0)), "Here is a snippet of code bro.\n```haskell\ninstance ToHtml Post where\n  toHtmlRaw = toHtml\n  toHtml Post{..} = div_ [class_ \"post\"] $ do\n    h1_ $ toHtml title\n    markdown\n    where\n      markdown = toHtml $ case MMark.parse (show title) body of\n        Left _  -> p_ \"invalid markdown\" -- should never run\n        Right m -> MMark.render m\n```\nDid you enjoy this code?")
      ]

getPostsPure :: Monad m => m [Post]
getPostsPure = pure . fmap snd . reverse $ Map.toList postsMap

getPostBySlugPure :: Monad m => Text -> m (Maybe Post)
getPostBySlugPure = pure . flip Map.lookup postsMap
