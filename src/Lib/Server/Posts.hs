module Lib.Server.Posts where

import           Lib.Effects.Logger (MonadLogger, info, withNamespace)
import           Lib.Effects.Post   (MonadPost, Post, getPosts)
import           Lucid.Extended     (Template (Template))
import           Protolude

getPostsHandler :: (MonadLogger m, MonadPost m) => m (Template [Post])
getPostsHandler = withNamespace "getPosts" $ do
  info "request for posts"
  Template "posts" <$> getPosts
