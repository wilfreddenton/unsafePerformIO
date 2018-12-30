{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Posts where

import           Data.Aeson.Extended (ToJSON, genericToJSON, object,
                                      snakeNoPrefix, toJSON, (.=))
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Effects.Post    (MonadPost, Post, getPostBySlug, getPosts)
import           Lib.Error           (AppError (AppError))
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

getPostHandler :: (MonadLogger m, MonadPost m, MonadError AppError m) => Text -> m (Template Post)
getPostHandler slug = withNamespace "getPost" . withContext (object ["slug" .= slug]) $ do
  info "request for post"
  postM <- getPostBySlug slug
  post <- case postM of
    Nothing -> throwError $ AppError "post not found"
    Just p  -> pure $ p
  pure $ Template "Post" post
