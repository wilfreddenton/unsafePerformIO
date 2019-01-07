{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Template where

import           Data.Aeson.Extended (ToJSON, toJSON)
import           Lib.Effects.Author  (About (..), Contact (..), Email (..),
                                      FacebookMessenger (..), Instagram (..),
                                      LinkedIn (..), MyLocation (..))
import           Lib.Effects.Post    (Post (..))
import           Lib.Env             (PgpKey (PgpKey))
import           Lucid.Extended
import           Protolude

data Template a = Template Text a deriving (Eq, Show)

instance ToJSON a => ToJSON (Template a) where
  toJSON (Template _ a) = toJSON a

instance ToHtml a => ToHtml (Template a) where
  toHtmlRaw = toHtml
  toHtml (Template title a) = doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/github.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      script_ [src_ "https://www.googletagmanager.com/gtag/js?id=UA-131857401-1", async_ "true"] ("" :: Text)
      script_ "window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'UA-131857401-1');"
      script_ [src_ "/static/js/highlight.pack.js"] ("" :: Text)
      script_ "hljs.initHighlightingOnLoad()"
    body_ $ do
      container_ . row_ $ do
        colMd4_ . nav_ $ do
          h1_ [id_ "title"] $ button_ [href_ "/"] "unsafePerformIO"
          p_ $ do
            span_ "[ Author: "
            a_ [class_ "hidden-link", href_ "/author"] "Wilfred Denton"
            span_ " ]"
          ul_ $ do
            li_ $ button_ [href_ "/about"] "About"
            li_ $ button_ [href_ "/contact"] "Contact"
            li_ $ button_ [href_ "https://github.com/wilfreddenton/resume/blob/master/wilfred_denton_resume.pdf", target_ "_blank"] "Resumé"
            li_ $ button_ [href_ "/pgp"] "PGP Key"
        colMd8_ . div_ [class_ "content"] $ toHtml a
      container_ [id_ "footer-container"] . footer_ $ do
        div_ $ span_ "BTC: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "ETH: xxxxxxxxxxxxxxxxxx"
        div_ $ span_ "XMR: xxxxxxxxxxxxxxxxxx"
        div_ [style_ "margin-top: 1rem"] $ do
          a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/3.0/us/"] $
            img_ [ alt_ "Creative Commons License"
                 , style_ "border-width:0"
                 , src_ "https://i.creativecommons.org/l/by-sa/3.0/us/80x15.png"
                 ]
          button_ [id_ "view-source", href_ "", (target_) "_blank"] "</>"

data AuthorTemplate = AuthorTemplate PgpKey [Post] (Maybe About) (Maybe Contact)

instance ToHtml AuthorTemplate where
  toHtmlRaw = toHtml
  toHtml (AuthorTemplate (PgpKey pgpKey) posts aboutM contactM) = doctypehtml_ $ do
    head_ $ do
      title_ "unsafePerformIO | author"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      script_ [src_ "/static/js/openpgp.min.js"] ("" :: Text)
      script_ [src_ "/static/js/author.js"] ("" :: Text)
      script_ $ "window.PUBLIC_KEY = `" <> pgpKey <> "`"
    body_ $ do
      container_ $ do
        row_ . col_ $ do
          h3_ "PGP Private Key"
          textarea_ [id_ "private-key", type_ "text", name_ "private-key", placeholder_ "PGP Private Key"] ""
          input_ [id_ "passphrase", type_ "text", name_ "passphrase", placeholder_ "Passphrase"]
        row_ . col_ $ do
          h3_ [class_ "toggle"] "Create Post"
          form_ [id_ "post-form", class_ "hidden"] $ do
            input_ [type_ "text", name_ "title", placeholder_ "Title"]
            textarea_ [type_ "text", name_ "body", placeholder_ "Body"] ""
            button_ [href_ ""] "submit"
        row_ . col_ $ do
          let editPostForm :: Monad m => Post -> HtmlT m ()
              editPostForm Post{..} = row_ . col_ $ do
                h3_ [class_ "toggle"] $ toHtml pTitle
                form_ [id_ "edit-form", class_ "hidden"] $ do
                  input_ [type_ "hidden", name_ "id", value_ . show $ fromMaybe 0 pId]
                  input_ [type_ "text", name_ "title", placeholder_ "Title", value_ pTitle]
                  textarea_ [type_ "text", name_ "body", placeholder_ "Body", value_ pBody] ""
                  button_ [href_ ""] "submit"
          h3_ [class_ "toggle"] "Edit Posts"
          div_ [class_ "hidden"] $ foldMap editPostForm posts
        row_ . col_ $ do
          h3_ [class_ "toggle"] "Edit About"
          form_ [id_ "about-form", class_ "hidden"] $ do
            let title = fromMaybe "" (aTitle <$> aboutM)
                body = fromMaybe "" (aBody <$> aboutM)
            input_ [type_ "text", name_ "title", placeholder_ "Title", value_ title]
            textarea_ [type_ "text", name_ "body", placeholder_ "Body", value_ body] ""
            button_ [href_ ""] "submit"
        row_ . col_ $ do
          h3_ [class_ "toggle"] "Edit Contact"
          form_ [id_ "contact-form", class_ "hidden"] $ do
            let MyLocation location = fromMaybe (MyLocation "") (cLocation <$> contactM)
                Email email = fromMaybe (Email "") (cEmail <$> contactM)
                LinkedIn linkedIn = fromMaybe (LinkedIn "") (cLinkedIn <$> contactM)
                FacebookMessenger facebookMessenger = fromMaybe (FacebookMessenger "") (cFacebookMessenger <$> contactM)
                Instagram instagram = fromMaybe (Instagram "") (cInstagram <$> contactM)
            input_ [type_ "text", name_ "location", placeholder_ "Location", value_ location]
            input_ [type_ "text", name_ "email", placeholder_ "Email", value_ email]
            input_ [type_ "text", name_ "linked_in", placeholder_ "LinkedIn", value_ linkedIn]
            input_ [type_ "text", name_ "facebook_messenger", placeholder_ "Facebook Messenger", value_ facebookMessenger]
            input_ [type_ "text", name_ "instagram", placeholder_ "Instagram", value_ instagram]
            button_ [href_ ""] "submit"
