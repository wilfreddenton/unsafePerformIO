{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}

module Lib.Effects.Post where

import           Data.Aeson.Extended (ToJSON, genericToJSON, snakeNoPrefix,
                                      toJSON)
import           Data.Time           (UTCTime (UTCTime), defaultTimeLocale,
                                      formatTime, fromGregorian,
                                      secondsToDiffTime)
import           Lib.Orphans         ()
import           Lucid.Extended      (ToHtml, class_, div_, h1_, h3_, href_,
                                      li_, p_, span_, termWith, toHtml,
                                      toHtmlRaw, ul_)
import           Protolude
import qualified Text.MMark          as MMark

-- Type
data Post = Post {
  pTitle     :: Text
, pCreatedAt :: UTCTime
, pBody      :: Text
} deriving (Eq, Show, Generic)

instance ToJSON Post where
  toJSON = genericToJSON snakeNoPrefix

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post{..} = div_ [class_ "post"] $ do
    h1_ $ toHtml pTitle
    markdown
    where
      markdown = toHtml $ case MMark.parse (show pTitle) pBody of
        Left _  -> p_ "invalid markdown" -- should never run
        Right m -> MMark.render m

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem Post {..} = li_ $ do
        termWith "a" [class_ "post-link", href_ ""] $ do
          _ <- h3_ $ do
            span_ [class_ "inset"] "_"
            toHtml $ pTitle
          span_ [class_ "post-list-date"] . toHtml . formatTime defaultTimeLocale "%b %d, %_Y" $ pCreatedAt

-- Typeclass
class Monad m => MonadPost m where
  getPosts :: m [Post]

-- Implementations

-- Pure
getPostsPure :: Monad m => m [Post]
getPostsPure = pure $
  [
    Post "HTML Templating with Lucid" (UTCTime (fromGregorian 2018 12 30) (secondsToDiffTime 0)) "blah blah blah!"
  , Post "Haskell JSON Tricks" (UTCTime (fromGregorian 2018 11 20) (secondsToDiffTime 0)) "blah blah blah!"
  , Post "Haskell is Frustratingly Good" (UTCTime (fromGregorian 2017 2 12) (secondsToDiffTime 0)) "*unsafePerformIO*"
  , Post "Foo" (UTCTime (fromGregorian 2008 8 22) (secondsToDiffTime 0)) "Let's go to the *Bar* ok?"
  , Post "Hello, World!" (UTCTime (fromGregorian 1994 1 31) (secondsToDiffTime 0)) "Here is a snippet of code bro.\n```haskell\ninstance ToHtml Post where\n  toHtmlRaw = toHtml\n  toHtml Post{..} = div_ [class_ \"post\"] $ do\n    h1_ $ toHtml title\n    markdown\n    where\n      markdown = toHtml $ case MMark.parse (show title) body of\n        Left _  -> p_ \"invalid markdown\" -- should never run\n        Right m -> MMark.render m\n```\nDid you enjoy this code?"
  ]
