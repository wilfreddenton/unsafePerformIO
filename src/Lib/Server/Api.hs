{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib.Server.Api where

import           Lib.Effects.Author (About, Contact, PgpKey)
import qualified Lib.Effects.Post   as P
import           Lib.Server.Posts   (PostPayload)
import           Lucid.Extended     (AuthorTemplate, Template)
import           Protolude
import           Servant            ((:<|>), (:>), Capture, Get, Header, JSON,
                                     Post, Raw, ReqBody)
import           Servant.HTML.Lucid (HTML)

type GetPosts = Get '[JSON, HTML] (Template [P.Post])

type API = GetPosts :<|>
  "posts" :> (
    GetPosts :<|>
    Capture "slug" Text :> Get '[JSON, HTML] (Template P.Post) :<|>
    Header "X-Author-Signature" Text :> ReqBody '[JSON] PostPayload :> Post '[JSON] ()
  ) :<|>
  "about" :> Get '[JSON, HTML] (Template About) :<|>
  "contact" :> Get '[JSON, HTML] (Template Contact) :<|>
  "pgp" :> Get '[JSON, HTML] (Template PgpKey) :<|>
  "author" :> Get '[HTML] AuthorTemplate :<|>
  "static" :> Raw :<|>
  Raw
