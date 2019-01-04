{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Posts where

import           Control.Lens        (( # ))
import           Crypto.Gpgme        (Protocol (OpenPGP), errorString,
                                      verifyDetached, withCtx)
import           Data.Aeson.Extended (FromJSON, ToJSON, genericParseJSON,
                                      genericToJSON, object, parseJSON,
                                      snakeNoPrefix, toJSON, (.=))
import qualified Data.Text.Encoding  as T
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Effects.Post    (MonadPost, Post, getPostBySlug, getPosts)
import           Lib.Error           (CanApiError, CanPostError, logAndThrow,
                                      _PostNotFoundError, _UnauthorizedError)
import           Lib.Server.Auth     (Signed (Signed))
import           Lucid.Extended      (Template (Template))
import           Protolude

data PostPayload = PostPayload {
  ppTitle :: Text
, ppBody  :: Text
} deriving Generic

instance ToJSON PostPayload where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON PostPayload where
  parseJSON = genericParseJSON snakeNoPrefix

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

getPostHandler :: (MonadLogger m, MonadPost m, CanPostError e m) => Text -> m (Template Post)
getPostHandler slug = withNamespace "getPost" . withContext (object ["slug" .= slug]) $ do
  info "request for post"
  postM <- getPostBySlug slug
  post <- case postM of
    Nothing -> logAndThrow $ _PostNotFoundError # slug
    Just p  -> pure $ p
  pure $ Template "Post" post

createPostHandler :: (MonadLogger m, MonadIO m, CanApiError e m) => Signed PostPayload -> m ()
createPostHandler (Signed sig PostPayload {..}) = withNamespace "createPost" . withContext (object ["title" .= ppTitle]) $ do
  info "request to create post"
  authorized <- liftIO $ withCtx "/home/wilfred/Documents/unsafePerformIO/.gnupg" "C" OpenPGP $ \ctx -> do
    resultE <- verifyDetached ctx (T.encodeUtf8 sig) (T.encodeUtf8 $ ppTitle <> ppBody)
    pure $ case resultE of
      Left _            -> False
      Right [(e, _, _)] -> if errorString e == "Success" then True else False
      Right _           -> False
  case authorized of
    False -> logAndThrow $ _UnauthorizedError # ()
    True  -> pure ()
