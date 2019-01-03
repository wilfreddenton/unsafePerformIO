{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib.Server (
  app
) where

import           Control.Lens          (( # ))
import           Data.Aeson.Extended   (object, (.=))
import qualified Data.ByteString.Char8 as BS
import           Data.Proxy            (Proxy (Proxy))
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
import           Lib.Server.Posts      (getPostHandler, getPostsHandler)
import           Lucid.Extended        (Template (Template))
import           Network.HTTP.Types    (mkStatus)
import           Network.Wai           (Application, rawPathInfo, responseLBS)
import           Protolude             hiding (log)
import           Servant

aboutHandler :: (MonadLogger m, MonadAuthor m) => m (Template About)
aboutHandler = withNamespace "about" $ do
  info "request for about"
  Template "About" <$> getAbout

contactHandler :: (MonadLogger m, MonadAuthor m) => m (Template Contact)
contactHandler = withNamespace "contact" $ do
  info "request for contact"
  Template "Contact" <$> getContact

pgpKeyHandler :: (MonadLogger m, MonadAuthor m) => m (Template PgpKey)
pgpKeyHandler = withNamespace "pgp" $ do
  info "request for pgp"
  Template "PGP Key" <$> getPgpKey

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
  (getPostsHandler :<|> getPostHandler) :<|>
  aboutHandler :<|>
  contactHandler :<|>
  pgpKeyHandler :<|>
  serveDirectoryWebApp "assets" :<|>
  notFoundHandler env

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = requestProvider $ \req -> serve api . hoistServer api (appToHandler env req) $ serverT env
  where requestProvider baseApp = \req res -> baseApp req req res
