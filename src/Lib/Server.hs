{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib.Server (
  app
) where

import           Control.Lens          (( # ))
import           Data.Aeson.Extended   (object, (.=))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map
import           Data.Proxy            (Proxy (Proxy))
import qualified Data.Text.Encoding    as T
import           Lib.App               (App, appToHandler, runLoggerT,
                                        toHttpError)
import           Lib.Effects.Logger    (infoKatip, withContextKatip,
                                        withNamespaceKatip)
import           Lib.Env               (AppEnv, HasLoggerEnv)
import           Lib.Error             (errorMessage, _NotFoundError)
import           Lib.Server.Api        (API)
import           Lib.Server.Author     (authorHandler, editAboutHandler,
                                        editContactHandler, getAboutHandler,
                                        getContactHandler, pgpKeyHandler)
import           Lib.Server.Posts      (createPostHandler, deletePostHandler,
                                        editPostHandler, getPostHandler,
                                        getPostsHandler)
import           Network.HTTP.Types    (hAccept, hContentType)
import           Network.HTTP.Types    (mkStatus)
import           Network.Wai           (Application, Request (requestHeaders),
                                        rawPathInfo, requestMethod, responseLBS)
import           Protolude             hiding (log)
import           Servant

notFoundHandler :: (HasLoggerEnv a) => a -> ServerT Raw m
notFoundHandler env = Tagged $ \req res -> do
  let route' = T.decodeUtf8 $ rawPathInfo req
      method = T.decodeUtf8 $ requestMethod req
      appErr = _NotFoundError # route'
  liftIO . runLoggerT env . withNamespaceKatip "notFound" . withContextKatip (object ["method" .= method, "route" .= route']) $
    infoKatip $ errorMessage appErr
  res . responseServantErr . toHttpError req $ appErr
  where
    responseServantErr ServantErr{..} =
      responseLBS (mkStatus errHTTPCode (BS.pack errReasonPhrase)) errHeaders errBody

serverT :: (HasLoggerEnv a) => a -> ServerT API App
serverT env =
  getPostsHandler :<|> (
    getPostsHandler :<|>
    getPostHandler :<|>
    createPostHandler :<|>
    editPostHandler :<|>
    deletePostHandler
  ) :<|> (
    getAboutHandler :<|>
    editAboutHandler
  ) :<|> (
    getContactHandler :<|>
    editContactHandler
  ) :<|>
  pgpKeyHandler :<|>
  authorHandler :<|>
  serveDirectoryWebApp "assets" :<|>
  notFoundHandler env

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = contentTypeAdjuster . requestProvider $ \req -> serve api . hoistServer api (appToHandler env req) $ serverT env
  where
    requestProvider baseApp = \req res -> baseApp req req res
    contentTypeAdjuster middleware = \req res ->
      let headers = requestHeaders req
          headersMap = Map.fromList headers
          headersNoAccept = Map.toList $ Map.delete hAccept headersMap
          contentTypeIsJson = maybe False (B.isInfixOf "application/json") $ Map.lookup hContentType headersMap
          acceptIsJson = maybe False (B.isInfixOf "application/json") $ Map.lookup hAccept headersMap
          adjustedHeaders =
            if | contentTypeIsJson && not acceptIsJson -> headersNoAccept <> [(hAccept, "application/json")]
               | not contentTypeIsJson && not acceptIsJson -> headersNoAccept <> [(hAccept, "text/html")]
               | otherwise -> headers
          adjustedReq = req { requestHeaders = adjustedHeaders }
      in middleware adjustedReq res
