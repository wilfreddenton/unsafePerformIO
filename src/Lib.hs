{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

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
import           Lucid              (ToHtml, class_, div_, h1_, h3_, li_, p_,
                                     toHtml, toHtmlRaw, ul_)
import           Network.Wai        (Application)
import           Protolude          hiding (log)
import           Servant            ((:>), Get, JSON, ServerT, hoistServer,
                                     serve)
import           Servant.HTML.Lucid (HTML)

data Post = Post {
  title :: Text
, body  :: Text
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post{..} = div_ [class_ "post"] $ do
    h1_ $ toHtml title
    p_ $ toHtml body

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem post = li_ $ do
        h3_ . toHtml $ title post

posts :: [Post]
posts = [ Post "Hello, World!" "blah blah blah"
        , Post "Foo" "Bar"
        ]

subHandler :: MonadLogger m => m ()
subHandler = withContext Null $ info "should be null"

getPosts :: MonadLogger m => m [Post]
getPosts = withNamespace "getPosts" $ do
  withContext posts $ info "request for posts"
  subHandler
  pure posts

type API = "posts" :> Get '[JSON, HTML] [Post]

serverT :: ServerT API App
serverT = getPosts

api :: Proxy API
api = Proxy

app :: AppEnv -> Application
app env = serve api $ hoistServer api (appToHandler env) serverT
