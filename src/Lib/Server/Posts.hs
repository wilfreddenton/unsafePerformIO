{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Posts where

import           Control.Lens        (( # ))
import           Data.Aeson.Extended (ToJSON, genericToJSON, object,
                                      snakeNoPrefix, toJSON, (.=))
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Effects.Post    (MonadPost, Post, getPostBySlug, getPosts)
import           Lib.Error           (AsPostError, logAndThrowError,
                                      _PostNotFoundError)
import           Lucid.Extended      (Template (Template))
import           Protolude

data PostPayload = PostPayload {
  ppTitle :: Text
, ppBody  :: Text
} deriving Generic

instance ToJSON PostPayload where
  toJSON = genericToJSON snakeNoPrefix

-- newPost :: MonadTime m => PostPayload -> m Post
-- newPost PostPayload {..} = do
--   createdAt <- now
--   pure $ Post makeSlug ppTitle createdAt ppBody
--   where
--     makeSlug = ""

getPostsHandler :: (MonadLogger m, MonadPost m) => m (Template [Post])
getPostsHandler = withNamespace "getPosts" $ do
  info "request for posts"
  Template "Posts" <$> getPosts

getPostHandler :: (MonadLogger m, MonadPost m, MonadError e m, AsPostError e, ToJSON e) => Text -> m (Template Post)
getPostHandler slug = withNamespace "getPost" . withContext (object ["slug" .= slug]) $ do
  info "request for post"
  postM <- getPostBySlug slug
  post <- case postM of
    Nothing -> logAndThrowError $ _PostNotFoundError # slug
    Just p  -> pure $ p
  pure $ Template "Post" post
