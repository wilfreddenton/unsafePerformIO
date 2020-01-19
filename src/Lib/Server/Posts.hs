{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Server.Posts where

import Control.Lens ((#))
import Data.Aeson.Extended
  ( (.=),
    FromJSON,
    ToJSON,
    Value,
    genericParseJSON,
    genericToJSON,
    object,
    parseJSON,
    snakeNoPrefix,
    toJSON,
  )
import Lib.Effects.Auth (MonadAuth, Signed (Signed), authorize)
import Lib.Effects.Logger
  ( MonadLogger,
    info,
    withContext,
    withNamespace,
  )
import Lib.Effects.Post
  ( MonadPost,
    Post (..),
    createPost,
    deletePost,
    editPost,
    getPostById,
    getPostBySlug,
    getPosts,
    makeSlug,
  )
import Lib.Effects.Time (MonadTime, now)
import Lib.Error
  ( CanPostError,
    _PostNotFoundError,
    _PostValidationError,
    logAndThrow,
    throwInvalid,
    validateLength,
    validateMinLength,
  )
import Lib.Server.Template (Template (Template))
import Lucid.Extended (extractMetaDescription)
import Protolude
import Servant (NoContent (NoContent))

data PostPayload
  = PostPayload
      { ppTitle :: Text,
        ppBody :: Text
      }
  deriving (Generic)

instance ToJSON PostPayload where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON PostPayload where
  parseJSON = genericParseJSON snakeNoPrefix

validatePostPayload :: (MonadLogger m, CanPostError e m) => PostPayload -> m ()
validatePostPayload PostPayload {..} =
  throwInvalid _PostValidationError $
    validateLength 3 280 "title" ppTitle <* validateMinLength 1 "body" ppBody

getPostsHandler :: (MonadLogger m, MonadPost m) => m (Template [Post])
getPostsHandler = withNamespace "getPosts" $ do
  info "request for posts"
  Template "unsafePerformIO" Nothing <$> getPosts

getPostHandler :: (MonadLogger m, MonadPost m, CanPostError e m) => Text -> m (Template Post)
getPostHandler slug = withNamespace "getPost" . withContext (object ["slug" .= slug]) $ do
  info "request for post"
  post@Post {..} <- maybe (logAndThrow $ _PostNotFoundError # slug) pure =<< getPostBySlug slug
  pure $ Template pTitle (Just $ extractMetaDescription 300 pBody) post

createPostHandler ::
  (MonadLogger m, MonadTime m, MonadPost m, MonadAuth m, CanPostError e m) =>
  Signed PostPayload ->
  m NoContent
createPostHandler (Signed sig pp@PostPayload {..}) =
  withNamespace "createPost" . withContext (object ["title" .= ppTitle]) $ do
    info "request to create post"
    authorize sig $ ppTitle <> ppBody
    validatePostPayload pp
    createdAt <- now
    createPost $ Post Nothing (makeSlug ppTitle createdAt) ppTitle createdAt ppBody
    pure NoContent

editPostHandler ::
  (MonadLogger m, MonadPost m, MonadAuth m, CanPostError e m) =>
  Int ->
  Signed PostPayload ->
  m NoContent
editPostHandler postId (Signed sig pp@PostPayload {..}) =
  withNamespace "editPost" . withContext (object ["id" .= postId]) $ do
    info "request to edit post"
    authorize sig $ ppTitle <> ppBody
    validatePostPayload pp
    post@Post {..} <- maybe (logAndThrow $ _PostNotFoundError # show postId) pure =<< getPostById postId
    editPost postId $ post {pTitle = ppTitle, pSlug = makeSlug ppTitle pCreatedAt, pBody = ppBody}
    pure NoContent

deletePostHandler ::
  (MonadLogger m, MonadPost m, MonadAuth m, CanPostError e m) =>
  Int ->
  Signed Value ->
  m NoContent
deletePostHandler postId (Signed sig _) =
  withNamespace "deletePost" . withContext (object ["id" .= postId]) $ do
    info "request to delete post"
    authorize sig $ show postId
    _ <- maybe (logAndThrow $ _PostNotFoundError # show postId) pure =<< getPostById postId
    deletePost postId
    pure NoContent
