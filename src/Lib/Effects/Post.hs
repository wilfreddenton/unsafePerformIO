{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.Effects.Post where

import           Data.Aeson     (FromJSON, ToJSON)
import           Lib.Orphans    ()
import           Lucid.Extended (ToHtml, class_, div_, h1_, li_, p_, toHtml,
                                 toHtmlRaw, ul_)
import           Protolude
import qualified Text.MMark     as MMark

-- Type
data Post = Post {
  title :: Text
, body  :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post{..} = div_ [class_ "post"] $ do
    h1_ $ toHtml title
    markdown
    where
      markdown = toHtml $ case MMark.parse (show title) body of
        Left _  -> p_ "invalid markdown" -- should never run
        Right m -> MMark.render m

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem post = li_ $ do
        toHtml post
        -- h3_ . toHtml $ title post

-- Typeclass
class Monad m => MonadPost m where
  getPosts :: m [Post]

-- Implementations

-- Pure
getPostsPure :: Monad m => m [Post]
getPostsPure = pure $
  [ Post "Hello, World!" "Here is a snippet of code bro.\n```haskell\ninstance ToHtml Post where\n  toHtmlRaw = toHtml\n  toHtml Post{..} = div_ [class_ \"post\"] $ do\n    h1_ $ toHtml title\n    markdown\n    where\n      markdown = toHtml $ case MMark.parse (show title) body of\n        Left _  -> p_ \"invalid markdown\" -- should never run\n        Right m -> MMark.render m\n```\nDid you enjoy this code?"
  , Post "Foo" "Let's go to the *Bar* ok?"
  , Post "Welcome to the Blog" "*unsafePerformIO*"
  ]
