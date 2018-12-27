{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Server.Api where

import           Lib.Effects.Post   (Post)
import           Lucid.Extended     (Template)
import           Servant            ((:>), Get, JSON)
import           Servant.HTML.Lucid (HTML)

type API = "posts" :> Get '[JSON, HTML] (Template [Post])
