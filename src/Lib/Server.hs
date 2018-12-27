{-# LANGUAGE ExplicitNamespaces #-}

module Lib.Server (
  app
) where

import           Data.Proxy       (Proxy (Proxy))
import           Lib.App          (App, appToHandler)
import           Lib.Env          (AppEnv)
import           Lib.Server.Api   (API)
import           Lib.Server.Posts (getPostsHandler)
import           Network.Wai      (Application)
import           Protolude        hiding (log)
import           Servant

serverT :: ServerT API App
serverT =
  getPostsHandler :<|>
  serveDirectoryWebApp "assets"

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appToHandler env) serverT
