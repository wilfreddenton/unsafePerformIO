{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib.Effects.Author where

import           Control.Lens                     (view)
import           Data.Aeson.Extended              (ToJSON, genericToJSON,
                                                   snakeNoPrefix, toJSON)
import           Database.SQLite.Simple           (Connection, FromRow, ToRow,
                                                   field, fromRow, query_,
                                                   toRow)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           Lib.Db                           (CanDb, liftDbAction)
import           Lib.Effects.Logger               (MonadLogger)
import           Lib.Env                          (dConn)
import           Lucid.Extended                   (ToHtml, a_, h3_, href_, li_,
                                                   renderMarkdown, strong_,
                                                   target_, toHtml, toHtmlRaw,
                                                   ul_)
import           Protolude

-- Types

newtype MyLocation = MyLocation Text deriving (Generic, ToField, FromField)

instance ToJSON MyLocation where
  toJSON (MyLocation location) = toJSON location

instance ToHtml MyLocation where
  toHtmlRaw = toHtml
  toHtml (MyLocation location)= toHtml location

newtype Email = Email Text deriving (Generic, ToField, FromField)

instance ToJSON Email where
  toJSON (Email email) = toJSON email

instance ToHtml Email where
  toHtmlRaw = toHtml
  toHtml (Email email)= a_ [href_ $ "mailto:" <> email, target_ "_blank"] $ toHtml email

newtype LinkedIn = LinkedIn Text deriving (Generic, ToField, FromField)

instance ToJSON LinkedIn where
  toJSON (LinkedIn username) = toJSON username

instance ToHtml LinkedIn where
  toHtmlRaw = toHtml
  toHtml (LinkedIn username) =
    a_ [href_ $ "https://www.linkedin.com/in/" <> username, target_ "_blank"] . toHtml $ "linkedin.com/in/" <> username

newtype FacebookMessenger = FacebookMessenger Text deriving (Generic, ToField, FromField)

instance ToJSON FacebookMessenger where
  toJSON (FacebookMessenger username) = toJSON username

instance ToHtml FacebookMessenger where
  toHtmlRaw = toHtml
  toHtml (FacebookMessenger username) =
    a_ [href_ $ "https://m.me/" <> username, target_ "_blank"] . toHtml $ "m.me/" <> username

newtype Instagram = Instagram Text deriving (Generic, ToField, FromField)

instance ToJSON Instagram where
  toJSON (Instagram username) = toJSON username

instance ToHtml Instagram where
  toHtmlRaw = toHtml
  toHtml (Instagram username) =
    a_ [href_ $ "https://www.instagram.com/" <> username, target_ "_blank"] . toHtml $ "instagram.com/" <> username

data Contact = Contact {
  dId                :: Int
, cLocation          :: MyLocation
, cEmail             :: Email
, cLinkedIn          :: LinkedIn
, cFacebookMessenger :: FacebookMessenger
, cInstagram         :: Instagram
} deriving Generic

instance FromRow Contact where
  fromRow = Contact <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Contact where
  toRow Contact {..} = toRow (dId, cLocation, cEmail, cLinkedIn, cFacebookMessenger, cInstagram)

instance ToJSON Contact where
  toJSON = genericToJSON snakeNoPrefix

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

data About = About {
  aId    :: Int
, aTitle :: Text
, aBody  :: Text
} deriving Generic

instance FromRow About where
  fromRow = About <$> field <*> field <*> field

instance ToRow About where
  toRow About {..} = toRow (aId, aTitle, aBody)

instance ToJSON About where
  toJSON = genericToJSON snakeNoPrefix

instance ToHtml About where
  toHtmlRaw = toHtml
  toHtml About {..} = do
    h3_ $ toHtml aTitle
    toHtml $ renderMarkdown aTitle aBody

-- Class

class Monad m => MonadAuthor m where
  getAbout :: m (Maybe About)
  getContact :: m (Maybe Contact)

-- Implementations

-- SQLite

onlyOneSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => (Connection -> IO [b]) -> m (Maybe b)
onlyOneSqlite action = do
  conn <- view dConn
  rows <- liftDbAction $ action conn
  pure $ case rows of
    []    -> Nothing
    [row] -> Just row
    _     -> Nothing

getAboutSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m (Maybe About)
getAboutSqlite = onlyOneSqlite $ action
  where action conn = (query_ conn "SELECT * FROM about" :: IO [About])

getContactSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m (Maybe Contact)
getContactSqlite = onlyOneSqlite $ action
  where action conn = (query_ conn "SELECT * FROM contact" :: IO [Contact])

-- Pure

getAboutPure :: Monad m => m (Maybe About)
getAboutPure = purer $ About 1 "I'm a full-stack software engineer currently working in New York City." "I've been working on private blockchain solutions for the financial services industry at [Symbiont.io](https://symbiont.io) since mid-2017. We hope to replace all the fax machines on Wall Street!\n\nThis blog is a collection of primarily technical posts. Writing the posts helps me further understand the topics and hopefully the posts themselves will be useful to readers. It is named after the Haskell function [`unsafePerformIO`](http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO). I think of each post as a call to this function. I put something out into the world unsure as to what might be thrown back at me."

getContactPure :: Monad m => m (Maybe Contact)
getContactPure = purer $ Contact 1 myLocation email linkedIn facebookMessenger instagram
  where
    myLocation = MyLocation "New York, New York"
    email = Email "dentonw3@gmail.com"
    linkedIn = LinkedIn "wilfreddenton"
    facebookMessenger = FacebookMessenger "wilfreddenton"
    instagram = Instagram "unsafe_perform_io"
