{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib.Server.Api where

import           Lib.Effects.Post   (Post)
import           Lib.Server.Pages   (About, Contact, PgpKey)
import           Lucid.Extended     (Template)
import           Protolude
import           Servant            ((:<|>), (:>), Capture, Get, JSON, Raw)
import           Servant.HTML.Lucid (HTML)

type GetPosts = Get '[JSON, HTML] (Template [Post])

type API = GetPosts :<|>
  "posts" :> (
    GetPosts :<|>
    Capture "slug" Text :> Get '[JSON, HTML] (Template Post)
  ) :<|>
  "about" :> Get '[JSON, HTML] (Template About) :<|>
  "contact" :> Get '[JSON, HTML] (Template Contact) :<|>
  "pgp" :> Get '[JSON, HTML] (Template PgpKey) :<|>
  "static" :> Raw :<|>
  Raw
