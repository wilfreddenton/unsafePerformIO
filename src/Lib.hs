{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Lib (
  app
) where

import           Data.Aeson         (FromJSON, ToJSON, Value (Null))
import           Data.Proxy         (Proxy (Proxy))
import           GHC.Generics       (Generic)
import           Lib.App            (App, appToHandler)
import           Lib.Effects.Logger (MonadLogger, info, withContext,
                                     withNamespace)
import           Lib.Env            (AppEnv)
import           Network.Wai        (Application)
import           Protolude          hiding (log)
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

subHandler :: MonadLogger m => m ()
subHandler = withContext Null $ info "should be null"

getPosts :: MonadLogger m => m [Post]
getPosts = do
  withNamespace "getPosts" $ do
    withContext posts $ info "request for posts"
    subHandler
    pure posts

type API = "posts" :> Get '[JSON] [Post]

serverT :: ServerT API App
serverT = getPosts

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appToHandler env) serverT
