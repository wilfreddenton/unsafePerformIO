{-# LANGUAGE ExplicitNamespaces #-}

module Lib.Server (
  app
) where

import           Control.Lens        (( # ))
import           Data.Aeson.Extended (object, (.=))
import           Data.Proxy          (Proxy (Proxy))
import           Lib.App             (App, appToHandler)
import           Lib.Effects.Logger  (MonadLogger, info, withContext,
                                      withNamespace)
import           Lib.Env             (AppEnv)
import           Lib.Error           (AppError, AsApiError, _NotFoundError)
import           Lib.Server.Api      (API)
import           Lib.Server.Pages    (aboutHandler, contactHandler,
                                      pgpKeyHandler)
import           Lib.Server.Posts    (getPostHandler, getPostsHandler)
import           Lucid.Extended      (Template)
import           Network.Wai         (Application)
import           Protolude           hiding (log)
import           Servant

notFoundHandler :: (MonadLogger m, MonadError e m, AsApiError e) => Text -> m (Template AppError)
notFoundHandler endpoint = withNamespace "notFound" . withContext (object ["route" .= endpoint]) $ do
  info "route not found"
  throwError $ _NotFoundError # endpoint

serverT :: ServerT API App
serverT =
  getPostsHandler :<|>
  (getPostsHandler :<|> getPostHandler) :<|>
  aboutHandler :<|>
  contactHandler :<|>
  pgpKeyHandler :<|>
  serveDirectoryWebApp "assets" :<|>
  notFoundHandler

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = requestProvider $ \req -> serve api $ hoistServer api (appToHandler env req) serverT
  where requestProvider baseApp = \req responseFunc -> baseApp req req responseFunc
