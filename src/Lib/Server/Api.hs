{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib.Server.Api where

import           Data.Aeson.Extended (Value)
import           Lib.Effects.Author  (About, Contact)
import qualified Lib.Effects.Post    as P
import           Lib.Env             (PgpKey)
import           Lib.Server.Auth     (Signed)
import           Lib.Server.Posts    (PostPayload)
import           Lucid.Extended      (AuthorTemplate, Template)
import           Protolude
import           Servant             ((:<|>), (:>), Capture, Delete, Get, JSON,
                                      NoContent, Post, Put, Raw, ReqBody)
import           Servant.HTML.Lucid  (HTML)

type GetPosts = Get '[JSON, HTML] (Template [P.Post])

type API = GetPosts :<|>
  "posts" :> (
    GetPosts :<|>
    Capture "slug" Text :> Get '[JSON, HTML] (Template P.Post) :<|>
    ReqBody '[JSON] (Signed PostPayload) :> Post '[JSON] NoContent :<|>
    Capture "id" Int :> ReqBody '[JSON] (Signed PostPayload) :> Put '[JSON] NoContent :<|>
    Capture "id" Int :> ReqBody '[JSON] (Signed Value) :> Delete '[JSON] NoContent
  ) :<|>
  "about" :> Get '[JSON, HTML] (Template About) :<|>
  "contact" :> Get '[JSON, HTML] (Template Contact) :<|>
  "pgp" :> Get '[JSON, HTML] (Template PgpKey) :<|>
  "author" :> Get '[HTML] AuthorTemplate :<|>
  "static" :> Raw :<|>
  Raw
