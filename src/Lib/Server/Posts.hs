{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Posts where

import           Control.Lens        (( # ))
import           Data.Aeson.Extended (FromJSON, ToJSON, Value, genericParseJSON,
                                      genericToJSON, object, parseJSON,
                                      snakeNoPrefix, toJSON, (.=))
import qualified Data.Text           as T
import           Lib.Effects.Auth    (MonadAuth, Signed (Signed), authorize)
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Effects.Post    (MonadPost, Post (..), createPost,
                                      deletePost, editPost, getPostById,
                                      getPostBySlug, getPosts, makeSlug)
import           Lib.Effects.Time    (MonadTime, now)
import           Lib.Error           (CanPostError, logAndThrow,
                                      _PostBodyEmptyError, _PostNotFoundError,
                                      _PostTitleEmptyError,
                                      _PostTitleTooLongError)
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

validatePostPayload :: (MonadLogger m, CanPostError e m) => PostPayload -> m ()
validatePostPayload PostPayload {..} = do
  if T.length ppTitle > 280 then (logAndThrow $ _PostTitleTooLongError # ()) else pure ()
  if T.length ppTitle == 0 then (logAndThrow $ _PostTitleEmptyError # ()) else pure ()
  if T.length ppBody == 0 then (logAndThrow $ _PostBodyEmptyError # ()) else pure ()

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

createPostHandler :: (MonadLogger m, MonadTime m, MonadPost m, MonadAuth m, CanPostError e m) => Signed PostPayload -> m NoContent
createPostHandler (Signed sig pp@PostPayload {..}) = withNamespace "createPost" . withContext (object ["title" .= ppTitle]) $ do
  info "request to create post"
  authorize sig $ ppTitle <> ppBody
  validatePostPayload pp
  createdAt <- now
  let slug = makeSlug ppTitle createdAt
  createPost $ Post Nothing slug ppTitle createdAt ppBody
  pure NoContent

editPostHandler :: (MonadLogger m, MonadPost m, MonadAuth m, CanPostError e m) => Int -> Signed PostPayload -> m NoContent
editPostHandler postId (Signed sig pp@PostPayload {..}) = withNamespace "editPost" . withContext (object ["id" .= postId]) $ do
  info "request to edit post"
  authorize sig $ ppTitle <> ppBody
  validatePostPayload pp
  postM <- getPostById postId
  post@Post {..} <- case postM of
    Nothing -> logAndThrow $ _PostNotFoundError # (show postId :: Text)
    Just p  -> pure p
  editPost postId $ post { pTitle = ppTitle, pSlug = makeSlug ppTitle pCreatedAt, pBody = ppBody }
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
