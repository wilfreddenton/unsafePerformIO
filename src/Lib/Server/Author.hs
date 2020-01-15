{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Server.Author where

import Control.Lens ((#), view)
import Data.Coerce (coerce)
import qualified Data.Text as T
import Lib.Effects.Auth (MonadAuth, Signed (Signed), authorize)
import Lib.Effects.Author
  ( About (..),
    Contact (..),
    Email (..),
    FacebookMessenger (..),
    Instagram (..),
    LinkedIn (..),
    MonadAuthor (..),
    MyLocation (..),
    editAbout,
    getAbout,
    getContact,
  )
import Lib.Effects.Logger (MonadLogger, info, withNamespace)
import Lib.Effects.Post (MonadPost, getPosts)
import Lib.Env (CanAuthEnv, PgpKey, aPgpKey)
import Lib.Error
  ( CanApiError,
    _FieldTooLongError,
    _NotFoundError,
    logAndThrow,
  )
import Lib.Server.Template (AuthorTemplate (..), Template (..))
import Protolude
import Servant (NoContent (NoContent))

type CanAuthor e m = (MonadLogger m, MonadAuthor m, CanApiError e m)

validateAbout :: (MonadLogger m, CanApiError e m) => About -> m ()
validateAbout About {..} = lengthCheck "title" aTitle
  where
    maxLen = 280
    lengthCheck fieldName field = if T.length field > maxLen then (logAndThrow $ _FieldTooLongError # fieldName) else pure ()

validateContact :: (MonadLogger m, CanApiError e m) => Contact -> m ()
validateContact Contact {..} = do
  lengthCheck "location" cLocation
  lengthCheck "location" cEmail
  lengthCheck "location" cLinkedIn
  lengthCheck "location" cFacebookMessenger
  lengthCheck "location" cInstagram
  where
    maxLen = 280
    lengthCheck fieldName field =
      if T.length (coerce field) > maxLen then (logAndThrow $ _FieldTooLongError # fieldName) else pure ()

baseHandler :: CanAuthor e m => Text -> m (Maybe a) -> m (Template a)
baseHandler title action = withNamespace loweredTitle $ do
  info $ "request for " <> title
  aM <- action
  case aM of
    Nothing -> logAndThrow $ _NotFoundError # loweredTitle
    Just a -> pure $ Template title a
  where
    loweredTitle = T.toLower title

getAboutHandler :: CanAuthor e m => m (Template About)
getAboutHandler = baseHandler "About" getAbout

editAboutHandler :: (CanAuthor e m, MonadAuth m) => Signed About -> m NoContent
editAboutHandler (Signed sig about@About {..}) = withNamespace "editAbout" $ do
  info "request to edit about"
  authorize sig $ aTitle <> aBody
  validateAbout about
  editAbout about
  pure NoContent

getContactHandler :: CanAuthor e m => m (Template Contact)
getContactHandler = baseHandler "Contact" getContact

editContactHandler :: (CanAuthor e m, MonadAuth m) => Signed Contact -> m NoContent
editContactHandler (Signed sig contact@Contact {..}) = withNamespace "editContact" $ do
  info "request to edit contact"
  authorize sig (l <> e <> li <> fm <> i)
  validateContact contact
  editContact contact
  pure NoContent
  where
    MyLocation l = cLocation
    Email e = cEmail
    LinkedIn li = cLinkedIn
    FacebookMessenger fm = cFacebookMessenger
    Instagram i = cInstagram

pgpKeyHandler :: (MonadLogger m, CanAuthEnv a m) => m (Template PgpKey)
pgpKeyHandler = withNamespace "pgp" $ do
  info "request for pgp"
  Template "PGP" <$> view aPgpKey

authorHandler :: (CanAuthor e m, CanAuthEnv a m, MonadPost m) => m AuthorTemplate
authorHandler = withNamespace "author" $ do
  info "request for author page"
  pgpKey <- view aPgpKey
  aboutM <- getAbout
  contactM <- getContact
  posts <- getPosts
  pure $ AuthorTemplate pgpKey posts aboutM contactM
