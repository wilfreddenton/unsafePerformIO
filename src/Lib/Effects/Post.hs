{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.Effects.Post where

import           Data.Aeson     (FromJSON, ToJSON)
import           Lucid.Extended (ToHtml, class_, div_, h1_, h3_, li_, p_,
                                 toHtml, toHtmlRaw, ul_)
import           Protolude

-- Type
data Post = Post {
  title :: Text
, body  :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post{..} = div_ [class_ "post"] $ do
    h1_ $ toHtml title
    p_ $ toHtml body

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem post = li_ $ do
        h3_ . toHtml $ title post

-- Typeclass
class Monad m => MonadPost m where
  getPosts :: m [Post]

-- Implementations

-- Pure

getPostsPure :: Monad m => m [Post]
getPostsPure = pure $
  [ Post "Hello, World!" "blah blah blah"
  , Post "Foo" "Bar"
  ]
