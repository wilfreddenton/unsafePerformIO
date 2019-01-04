{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Posts where

import           Control.Lens        (( # ))
import           Data.Aeson.Extended (FromJSON, ToJSON, Value, genericParseJSON,
                                      genericToJSON, object, parseJSON,
                                      snakeNoPrefix, toJSON, (.=))
import           Lib.Effects.Auth    (MonadAuth, authorize)
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Effects.Post    (MonadPost, Post (Post), createPost,
                                      deletePost, getPostById, getPostBySlug,
                                      getPosts, makeSlug)
import           Lib.Effects.Time    (MonadTime, now)
import           Lib.Error           (CanPostError, logAndThrow,
                                      _PostNotFoundError)
import           Lib.Server.Auth     (Signed (Signed))
import           Lucid.Extended      (Template (Template))
import           Protolude
import           Servant             (NoContent (NoContent))

data PostPayload = PostPayload {
  ppTitle :: Text
, ppBody  :: Text
} deriving Generic

instance ToJSON PostPayload where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON PostPayload where
  parseJSON = genericParseJSON snakeNoPrefix

getPostsHandler :: (MonadLogger m, MonadPost m) => m (Template [Post])
getPostsHandler = withNamespace "getPosts" $ do
  info "request for posts"
  Template "Posts" <$> getPosts

getPostHandler :: (MonadLogger m, MonadPost m, CanPostError e m) => Text -> m (Template Post)
getPostHandler slug = withNamespace "getPost" . withContext (object ["slug" .= slug]) $ do
  info "request for post"
  postM <- getPostBySlug slug
  post <- case postM of
    Nothing -> logAndThrow $ _PostNotFoundError # slug
    Just p  -> pure $ p
  pure $ Template "Post" post

createPostHandler :: (MonadLogger m, MonadTime m, MonadPost m, MonadAuth m) => Signed PostPayload -> m NoContent
createPostHandler (Signed sig PostPayload {..}) = withNamespace "createPost" . withContext (object ["title" .= ppTitle]) $ do
  info "request to create post"
  authorize sig $ ppTitle <> ppBody
  createdAt <- now
  let slug = makeSlug ppTitle createdAt
  createPost $ Post Nothing ppTitle slug createdAt ppBody
  pure NoContent

deletePostHandler :: (MonadLogger m, MonadPost m, MonadAuth m, CanPostError e m) => Int -> Signed Value -> m NoContent
deletePostHandler postId (Signed sig _) = withNamespace "deletePost" . withContext (object ["id" .= postId]) $ do
  info "request to delete post"
  authorize sig postIdText
  postM <- getPostById postId
  case postM of
    Nothing -> logAndThrow $ _PostNotFoundError # postIdText
    Just _  -> pure ()
  deletePost postId
  pure NoContent
  where postIdText = show postId :: Text
