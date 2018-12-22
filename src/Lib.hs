{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib (
  app
) where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Proxy         (Proxy (Proxy))
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Lib.App            (App, appToHandler)
import           Lib.Effects.Logger (MonadLogger, log)
import           Lib.Env            (AppEnv)
import           Network.Wai        (Application)
import           Prelude            hiding (log)
import           Servant            ((:>), Get, JSON, ServerT, hoistServer,
                                     serve)

data Post = Post {
  title :: Text
, body  :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

posts :: [Post]
posts = [ Post "Hello, World!" "blah blah blah"
        , Post "Foo" "Bar"
        ]

getPosts :: MonadLogger m => m [Post]
getPosts = do
  log "request for posts"
  pure posts

type API = "posts" :> Get '[JSON] [Post]

serverT :: ServerT API App
serverT = getPosts

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appToHandler env) serverT
