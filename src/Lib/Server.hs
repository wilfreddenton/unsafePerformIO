{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib.Server (
  app
) where

import           Control.Lens          (( # ))
import           Data.Aeson.Extended   (object, (.=))
import qualified Data.ByteString.Char8 as BS
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Lib.App               (App, appToHandler, runLoggerT)
import           Lib.Effects.Author    (About, Contact, MonadAuthor, PgpKey,
                                        getAbout, getContact, getPgpKey)
import           Lib.Effects.Logger    (MonadLogger, info, infoKatip,
                                        withContextKatip, withNamespace,
                                        withNamespaceKatip)
import           Lib.Env               (AppEnv, HasLoggerEnv)
import           Lib.Error             (errorMessage, toHttpError,
                                        _NotFoundError)
import           Lib.Server.Api        (API)
import           Lib.Server.Posts      (createPostHandler, getPostHandler,
                                        getPostsHandler)
import           Lucid.Extended        (AuthorTemplate (AuthorTemplate),
                                        Template (Template))
import           Network.HTTP.Types    (mkStatus)
import           Network.Wai           (Application, rawPathInfo, responseLBS)
import           Protolude             hiding (log)
import           Servant

type CanAuthor m = (MonadLogger m, MonadAuthor m)

baseHandler :: (CanAuthor m) => Text -> m a -> m (Template a)
baseHandler title action = withNamespace (T.toLower title) $ do
  info $ "request for " <> title
  Template title <$> action

aboutHandler :: CanAuthor m => m (Template About)
aboutHandler = baseHandler "About" getAbout

contactHandler :: CanAuthor m => m (Template Contact)
contactHandler = baseHandler "Contact" getContact

pgpKeyHandler :: CanAuthor m => m (Template PgpKey)
pgpKeyHandler = baseHandler "PGP" getPgpKey

authorHandler :: (MonadLogger m) => m AuthorTemplate
authorHandler = pure AuthorTemplate

notFoundHandler :: (HasLoggerEnv a) => a -> ServerT Raw m
notFoundHandler env = Tagged $ \req res -> do
  let route' = T.decodeUtf8 $ rawPathInfo req
      appErr = _NotFoundError # route'
  liftIO . runLoggerT env . withNamespaceKatip "notFound" . withContextKatip (object ["route" .= route']) $
    infoKatip $ errorMessage appErr
  res . responseServantErr . toHttpError req $ appErr
  where
    responseServantErr ServantErr{..} =
      responseLBS (mkStatus errHTTPCode (BS.pack errReasonPhrase)) errHeaders errBody

serverT :: (HasLoggerEnv a) => a -> ServerT API App
serverT env =
  getPostsHandler :<|>
  (getPostsHandler :<|> getPostHandler :<|> createPostHandler) :<|>
  aboutHandler :<|>
  contactHandler :<|>
  pgpKeyHandler :<|>
  authorHandler :<|>
  serveDirectoryWebApp "assets" :<|>
  notFoundHandler env

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = requestProvider $ \req -> serve api . hoistServer api (appToHandler env req) $ serverT env
  where requestProvider baseApp = \req res -> baseApp req req res
