{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib.Server.Api where

import           Lib.Effects.Post   (Post)
import           Lib.Server.Pages   (Contact)
import           Lucid.Extended     (Template)
import           Servant            ((:<|>), (:>), Get, JSON, Raw)
import           Servant.HTML.Lucid (HTML)

type API = Get '[JSON, HTML] (Template [Post]) :<|>
  "contact" :> Get '[JSON, HTML] (Template Contact) :<|>
  "static" :> Raw
