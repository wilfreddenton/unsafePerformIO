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
      let description = "unsafePerformIO is the blog and personal website of Wilfred Denton."
      title_ $ toHtml title
      meta_ [charset_ "utf-8"]
      meta_ [name_ "description", content_ description]
      meta_ [name_ "author", content_ "Wilfred Denton"]
      meta_ [ name_ "keywords"
            , content_ "code, computer, engineer, engineering, functional, haskell, programming, science, software, technology"
            ]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      meta_ [property_ "og:title", content_ title]
      meta_ [property_ "og:type", content_ "website"]
      meta_ [property_ "og:description", content_ description]
      meta_ [property_ "og:image", content_ "https://unsafe-perform.io/static/img/favicon.png"]
      meta_ [property_ "og:url", content_ "https://unsafe-perform.io"]
      meta_ [name_ "twitter:title", content_ title]
      meta_ [name_ "twitter:description", content_ description]
      meta_ [name_ "twitter:image", content_ "https://unsafe-perform.io/static/img/favicon.png"]
      meta_ [name_ "twitter:card", content_ "summary"]
      link_ [rel_ "icon", type_ "image/png", href_ "/static/img/favicon.png"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/style.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/css/bootstrap-grid.min.css"]
      script_ [src_ "/static/js/script.js", async_ "true"] ("" :: Text)
      script_ [src_ "https://www.googletagmanager.com/gtag/js?id=UA-131857401-1", async_ "true"] ("" :: Text)
      script_ "window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'UA-131857401-1');"
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
        p_ [id_ "monero"] $ do
          a_ [href_ "https://www.getmonero.org/", target_ "_blank"] "XMR"
          span_ ": "
          code_ "44e8Sw7PBudHZ5BQuSEEwKA6B5U92fzSPWyVhExNgsvu4i4iZeYzRCrZc5NbJxeNdY8sMVZ2fmvxx97Dg6s74hWH8QkMgR4"
        div_ [style_ "margin-top: 1rem"] $ do
          a_ [rel_ "license", href_ "http://creativecommons.org/licenses/by-sa/3.0/us/"] $
            img_ [ alt_ "Creative Commons License"
                 , style_ "border-width:0"
                 , src_ "https://i.creativecommons.org/l/by-sa/3.0/us/80x15.png"
                 ]
          button_ [id_ "view-source", href_ "https://github.com/wilfreddenton/unsafePerformIO", target_ "_blank"] "</>"

data AuthorTemplate = AuthorTemplate PgpKey [Post] (Maybe About) (Maybe Contact)

instance ToHtml AuthorTemplate where
  toHtmlRaw = toHtml
  toHtml (AuthorTemplate (PgpKey pgpKey) posts aboutM contactM) = doctypehtml_ $ do
    head_ $ do
      title_ "Authoring"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "icon", type_ "image/png", href_ "/static/img/favicon.png"]
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
          input_ [id_ "passphrase", type_ "password", name_ "passphrase", placeholder_ "Passphrase"]
        row_ . col_ $ do
          h3_ [class_ "toggle"] "Create Post"
          form_ [id_ "post-form", class_ "hidden"] $ do
            input_ [type_ "text", name_ "title", placeholder_ "Title"]
            textarea_ [type_ "text", name_ "body", placeholder_ "Body"] ""
            button_ [href_ ""] "submit"
        row_ . col_ $ do
          let editPostForm :: Monad m => Post -> HtmlT m ()
              editPostForm Post{..} = row_ . col_ $ do
                h3_ [class_ "toggle"] . toHtml $ "_" <> pTitle
                let id = show $ fromMaybe 0 pId
                form_ [id_ ("editpost-" <> id <> "-form"), class_ "hidden"] $ do
                  button_ [href_ ""] "delete"
                  input_ [type_ "hidden", name_ "postId", value_ id]
                  input_ [type_ "text", name_ "title", placeholder_ "Title", value_ pTitle]
                  textarea_ [type_ "text", name_ "body", placeholder_ "Body"] $ toHtml pBody
                  button_ [href_ ""] "submit"
          h3_ [class_ "toggle"] "Edit Posts"
          div_ [class_ "hidden"] $ foldMap editPostForm posts
        row_ . col_ $ do
          h3_ [class_ "toggle"] "Edit About"
          form_ [id_ "about-form", class_ "hidden"] $ do
            let title = fromMaybe "" (aTitle <$> aboutM)
                body = fromMaybe "" (aBody <$> aboutM)
            input_ [type_ "text", name_ "title", placeholder_ "Title", value_ title]
            textarea_ [type_ "text", name_ "body", placeholder_ "Body"] $ toHtml body
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
            input_ [type_ "text", name_ "linkedIn", placeholder_ "LinkedIn", value_ linkedIn]
            input_ [type_ "text", name_ "facebookMessenger", placeholder_ "Facebook Messenger", value_ facebookMessenger]
            input_ [type_ "text", name_ "instagram", placeholder_ "Instagram", value_ instagram]
            button_ [href_ ""] "submit"
