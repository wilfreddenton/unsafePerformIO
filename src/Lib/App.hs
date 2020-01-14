{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib.App where

import Control.Lens ((^.))
import Crypto.Random (MonadRandom, getRandomBytes)
import Data.Aeson.Extended (encode, toJSON)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Katip
  ( Katip,
    KatipContext,
    KatipContextT,
    runKatipContextT,
  )
import Lib.Effects.Auth (MonadAuth, authorize, authorizeIO)
import Lib.Effects.Author
  ( MonadAuthor,
    editAbout,
    editAboutSqlite,
    editContact,
    editContactSqlite,
    getAbout,
    getAboutSqlite,
    getContact,
    getContactSqlite,
  )
import Lib.Effects.Logger
  ( MonadLogger (..),
    debugKatip,
    errorKatip,
    infoKatip,
    warnKatip,
    withContextKatip,
    withNamespaceKatip,
  )
import Lib.Effects.Post
  ( MonadPost,
    createPost,
    createPostSqlite,
    deletePost,
    deletePostSqlite,
    editPost,
    editPostSqlite,
    getPostById,
    getPostByIdSqlite,
    getPostBySlug,
    getPostBySlugSqlite,
    getPosts,
    getPostsSqlite,
  )
import Lib.Effects.Random (getRandomBytesIO)
import Lib.Effects.Time (MonadTime, now, nowIO)
import Lib.Env
  ( AppEnv,
    HasLoggerEnv,
    lContext,
    lLogEnv,
    lNamespace,
  )
import Lib.Error (AppError, httpStatus)
import Lib.Server.Template (Template (Template))
import Lucid.Extended (renderBS, toHtml)
import Network.HTTP.Types
  ( Status (Status),
    hAccept,
    hContentType,
    statusCode,
    statusMessage,
  )
import Network.Wai (Request, requestHeaders)
import Protolude
import Servant (Handler, ServerError (ServerError))

newtype App a
  = App
      { unApp :: KatipContextT (ReaderT AppEnv (ExceptT AppError IO)) a
      }
  deriving (Monad, Functor, Applicative, MonadReader AppEnv, MonadError AppError, MonadIO, Katip, KatipContext)

instance MonadLogger App where

  debug = debugKatip

  error = errorKatip

  info = infoKatip

  warn = warnKatip

  withNamespace = withNamespaceKatip

  withContext = withContextKatip

instance MonadTime App where
  now = nowIO

instance MonadRandom App where
  getRandomBytes = getRandomBytesIO

instance MonadAuth App where
  authorize = authorizeIO

instance MonadPost App where

  getPosts = getPostsSqlite

  getPostById = getPostByIdSqlite

  getPostBySlug = getPostBySlugSqlite

  createPost = createPostSqlite

  editPost = editPostSqlite

  deletePost = deletePostSqlite

instance MonadAuthor App where

  getAbout = getAboutSqlite

  editAbout = editAboutSqlite

  getContact = getContactSqlite

  editContact = editContactSqlite

runLoggerT :: HasLoggerEnv e => e -> KatipContextT m a -> m a
runLoggerT env = runKatipContextT (env ^. lLogEnv) (env ^. lContext) (env ^. lNamespace)

toHttpError :: Request -> AppError -> ServerError
toHttpError req appErr =
  let headersMap = Map.fromList $ requestHeaders req
      acceptHeaderM = Map.lookup hAccept headersMap
      Status {statusCode, statusMessage} = httpStatus appErr
      jsonTuple = (encode . toJSON, (hContentType, "application/json"))
      htmlTuple = (renderBS . toHtml . Template "error", (hContentType, "text/html"))
      (toBS, contentTypeHeader) = case acceptHeaderM of
        Nothing -> jsonTuple
        Just accept -> if B.isInfixOf "application/json" accept then jsonTuple else htmlTuple
   in ServerError (statusCode) (show $ statusMessage) (toBS appErr) [contentTypeHeader]

appToHandler :: AppEnv -> Request -> App a -> Handler a
appToHandler env req app = do
  res <- liftIO . runExceptT . flip runReaderT env . runLoggerT env $ unApp app
  either (\e -> throwError (toHttpError req e)) pure res
