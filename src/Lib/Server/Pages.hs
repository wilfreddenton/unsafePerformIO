{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Lib.Server.Pages where

import           Data.Aeson.Extended (ToJSON, genericToJSON, snakeNoPrefix,
                                      toJSON)
import           Lucid.Extended      (Template (Template))
import           Lucid.Extended
import           Protolude

newtype MyLocation = MyLocation Text deriving Generic

instance ToJSON MyLocation where
  toJSON (MyLocation location) = toJSON location

instance ToHtml MyLocation where
  toHtmlRaw = toHtml
  toHtml (MyLocation location)= toHtml location

newtype Email = Email Text deriving Generic

instance ToJSON Email where
  toJSON (Email email) = toJSON email

instance ToHtml Email where
  toHtmlRaw = toHtml
  toHtml (Email email)= a_ [href_ $ "mailto:" <> email, target_ "_blank"] $ toHtml email

newtype LinkedIn = LinkedIn Text deriving Generic

instance ToJSON LinkedIn where
  toJSON (LinkedIn username) = toJSON username

instance ToHtml LinkedIn where
  toHtmlRaw = toHtml
  toHtml (LinkedIn username) =
    a_ [href_ $ "https://www.linkedin.com/in/" <> username, target_ "_blank"] . toHtml $ "linkedin.com/in/" <> username

newtype FacebookMessenger = FacebookMessenger Text deriving Generic

instance ToJSON FacebookMessenger where
  toJSON (FacebookMessenger username) = toJSON username

instance ToHtml FacebookMessenger where
  toHtmlRaw = toHtml
  toHtml (FacebookMessenger username) =
    a_ [href_ $ "https://m.me/" <> username, target_ "_blank"] . toHtml $ "m.me/" <> username

newtype Instagram = Instagram Text deriving Generic

instance ToJSON Instagram where
  toJSON (Instagram username) = toJSON username

instance ToHtml Instagram where
  toHtmlRaw = toHtml
  toHtml (Instagram username) =
    a_ [href_ $ "https://www.instagram.com/" <> username, target_ "_blank"] . toHtml $ "instagram.com/" <> username

data Contact = Contact {
  cLocation          :: MyLocation
, cEmail             :: Email
, cLinkedIn          :: LinkedIn
, cFacebookMessenger :: FacebookMessenger
, cInstagram         :: Instagram
} deriving Generic

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

contactHandler :: Monad m => m (Template Contact)
contactHandler = pure . Template "contact" $ Contact myLocation email linkedIn facebookMessenger instagram
  where
    myLocation = MyLocation "New York, New York"
    email = Email "dentonw3@gmail.com"
    linkedIn = LinkedIn "wilfreddenton"
    facebookMessenger = FacebookMessenger "wilfreddenton"
    instagram = Instagram "unsafe_perform_io"
