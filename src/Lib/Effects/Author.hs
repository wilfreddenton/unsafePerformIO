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
                                                   pre_, renderMarkdown,
                                                   strong_, target_, toHtml,
                                                   toHtmlRaw, ul_)
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

data PgpKey = PgpKey {
  pkId     :: Int
, pkPgpKey :: Text
} deriving Generic

instance FromRow PgpKey where
  fromRow = PgpKey <$> field <*> field

instance ToRow PgpKey where
  toRow PgpKey {..} = toRow (pkId, pkPgpKey)

instance ToJSON PgpKey where
  toJSON = genericToJSON snakeNoPrefix

instance ToHtml PgpKey where
  toHtmlRaw = toHtml
  toHtml PgpKey {..}= pre_ $ toHtml pkPgpKey

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
  getPgpKey :: m (Maybe PgpKey)

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

getPgpKeySqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m (Maybe PgpKey)
getPgpKeySqlite = onlyOneSqlite $ action
  where action conn = (query_ conn "SELECT * FROM pgp" :: IO [PgpKey])

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

getPgpKeyPure :: Monad m => m (Maybe PgpKey)
getPgpKeyPure = purer $ PgpKey 1 "-----BEGIN PGP PUBLIC KEY BLOCK-----\n\n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\n=7mVf\n-----END PGP PUBLIC KEY BLOCK-----\n"
