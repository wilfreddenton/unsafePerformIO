{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Effects.Author where

import Control.Lens (view)
import Data.Aeson.Extended
  ( FromJSON,
    ToJSON,
    genericParseJSON,
    genericToJSON,
    parseJSON,
    snakeNoPrefix,
    toJSON,
  )
import Database.SQLite.Simple
  ( FromRow,
    NamedParam ((:=)),
    ToRow,
    execute,
    executeNamed,
    field,
    fromRow,
    query_,
    toRow,
  )
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import Lib.Db (CanDb, liftDbAction, onlyOneSqlite)
import Lib.Effects.Logger (MonadLogger)
import Lib.Env (dConn)
import Lucid.Extended
  ( ToHtml,
    a_,
    h3_,
    href_,
    li_,
    renderMarkdown,
    strong_,
    target_,
    toHtml,
    toHtmlRaw,
    ul_,
  )
import Protolude

-- Types

newtype MyLocation = MyLocation Text
  deriving (Eq, Show, Generic, ToField, FromField, ToJSON, FromJSON)

instance ToHtml MyLocation where

  toHtmlRaw = toHtml

  toHtml (MyLocation location) = toHtml location

newtype Email = Email Text
  deriving (Eq, Show, Generic, ToField, FromField, ToJSON, FromJSON)

instance ToHtml Email where

  toHtmlRaw = toHtml

  toHtml (Email email) = a_ [href_ $ "mailto:" <> email, target_ "_blank"] $ toHtml email

newtype LinkedIn = LinkedIn Text
  deriving (Eq, Show, Generic, ToField, FromField, ToJSON, FromJSON)

instance ToHtml LinkedIn where

  toHtmlRaw = toHtml

  toHtml (LinkedIn username) =
    a_ [href_ $ "https://www.linkedin.com/in/" <> username, target_ "_blank"]
      . toHtml
      $ "linkedin.com/in/" <> username

newtype FacebookMessenger = FacebookMessenger Text
  deriving (Eq, Show, Generic, ToField, FromField, ToJSON, FromJSON)

instance ToHtml FacebookMessenger where

  toHtmlRaw = toHtml

  toHtml (FacebookMessenger username) =
    a_ [href_ $ "https://m.me/" <> username, target_ "_blank"] . toHtml $ "m.me/" <> username

newtype Instagram = Instagram Text
  deriving (Eq, Show, Generic, ToField, FromField, ToJSON, FromJSON)

instance ToHtml Instagram where

  toHtmlRaw = toHtml

  toHtml (Instagram username) =
    a_ [href_ $ "https://www.instagram.com/" <> username, target_ "_blank"]
      . toHtml
      $ "instagram.com/" <> username

data Contact
  = Contact
      { cLocation :: MyLocation,
        cEmail :: Email,
        cLinkedIn :: LinkedIn,
        cFacebookMessenger :: FacebookMessenger,
        cInstagram :: Instagram
      }
  deriving (Eq, Show, Generic)

instance FromRow Contact where
  fromRow = Contact <$> field <*> field <*> field <*> field <*> field

instance ToRow Contact where
  toRow Contact {..} = toRow (cLocation, cEmail, cLinkedIn, cFacebookMessenger, cInstagram)

instance ToJSON Contact where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON Contact where
  parseJSON = genericParseJSON snakeNoPrefix

instance ToHtml Contact where

  toHtmlRaw = toHtml

  toHtml Contact {..} = ul_ $ do
    contactListItem "Location" cLocation
    contactListItem "Email" cEmail
    contactListItem "LinkedIn" cLinkedIn
    contactListItem "FB Messenger" cFacebookMessenger
    contactListItem "Instagram" cInstagram
    where
      contactListItem k v = li_ $ do
        strong_ $ k <> ": "
        toHtml v

data About
  = About
      { aTitle :: Text,
        aBody :: Text
      }
  deriving (Eq, Show, Generic)

instance FromRow About where
  fromRow = About <$> field <*> field

instance ToRow About where
  toRow About {..} = toRow (aTitle, aBody)

instance ToJSON About where
  toJSON = genericToJSON snakeNoPrefix

instance FromJSON About where
  parseJSON = genericParseJSON snakeNoPrefix

instance ToHtml About where

  toHtmlRaw = toHtml

  toHtml About {..} = do
    h3_ $ toHtml aTitle
    toHtmlRaw $ renderMarkdown aBody

-- Class

class Monad m => MonadAuthor m where

  getAbout :: m (Maybe About)

  editAbout :: About -> m ()

  getContact :: m (Maybe Contact)

  editContact :: Contact -> m ()

-- Implementations

-- SQLite

getAboutSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m (Maybe About)
getAboutSqlite = onlyOneSqlite $ flip query_ "SELECT title, body FROM about"

editAboutSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => About -> m ()
editAboutSqlite about = do
  conn <- view dConn
  aboutM <- getAboutSqlite
  liftDbAction $ case aboutM of
    Nothing -> execute conn "INSERT INTO about (title, body) VALUES (?, ?)" about
    Just _ ->
      executeNamed
        conn
        "UPDATE about SET title = :title, body = :body WHERE id = 1"
        [ ":title" := aTitle about,
          ":body" := aBody about
        ]

getContactSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m (Maybe Contact)
getContactSqlite =
  onlyOneSqlite $
    flip query_ "SELECT location, email, linked_in, facebook_messenger, instagram FROM contact"

editContactSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Contact -> m ()
editContactSqlite contact@Contact {..} = do
  conn <- view dConn
  contactM <- getContactSqlite
  liftDbAction $ case contactM of
    Nothing -> execute conn "INSERT INTO contact (location, email, linked_in, facebook_messenger, instagram) VALUES (?, ?, ?, ?, ?)" contact
    Just _ ->
      executeNamed
        conn
        "UPDATE contact SET location = :l, email = :e, linked_in = :li, facebook_messenger = :fm, instagram = :in WHERE id = 1"
        [ ":l" := cLocation,
          ":e" := cEmail,
          ":li" := cLinkedIn,
          ":fm" := cFacebookMessenger,
          ":in" := cInstagram
        ]

-- Pure

getAboutPure :: Monad m => m (Maybe About)
getAboutPure = purer $ About "I'm a full-stack software engineer currently working in New York City." "I've been working on private blockchain solutions for the financial services industry at [Symbiont.io](https://symbiont.io) since mid-2017. We hope to replace all the fax machines on Wall Street!\n\nThis blog is a collection of primarily technical posts. Writing the posts helps me further understand the topics and hopefully the posts themselves will be useful to readers. It is named after the Haskell function [`unsafePerformIO`](http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO). I think of each post as a call to this function. I put something out into the world unsure as to what might be thrown back at me."

getContactPure :: Monad m => m (Maybe Contact)
getContactPure = purer $ Contact myLocation email linkedIn facebookMessenger instagram
  where
    myLocation = MyLocation "New York, New York"
    email = Email "dentonw3@gmail.com"
    linkedIn = LinkedIn "wilfreddenton"
    facebookMessenger = FacebookMessenger "wilfreddenton"
    instagram = Instagram "unsafe_perform_io"
