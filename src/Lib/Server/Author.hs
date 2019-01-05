{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Server.Author where

import           Control.Lens       (view, ( # ))
import qualified Data.Text          as T
import           Lib.Effects.Auth   (MonadAuth, Signed (Signed), authorize)
import           Lib.Effects.Author (About (..), Contact (..), Email (..),
                                     FacebookMessenger (..), Instagram (..),
                                     LinkedIn (..), MonadAuthor (..),
                                     MyLocation (..), editAbout, getAbout,
                                     getContact)
import           Lib.Effects.Logger (MonadLogger, info, withNamespace)
import           Lib.Env            (CanAuthEnv, PgpKey (PgpKey), aPgpKey)
import           Lib.Error          (CanApiError, logAndThrow, _NotFoundError)
import           Lucid.Extended     (AuthorTemplate (..), Template (..))
import           Protolude
import           Servant            (NoContent (NoContent))

type CanAuthor e m = (MonadLogger m, MonadAuthor m, CanApiError e m)

baseHandler :: CanAuthor e m => Text -> m (Maybe a) -> m (Template a)
baseHandler title action = withNamespace loweredTitle $ do
  info $ "request for " <> title
  aM <- action
  case aM of
    Nothing -> logAndThrow $ _NotFoundError # loweredTitle
    Just a  -> pure $ Template title a
  where loweredTitle = T.toLower title

aboutHandler :: CanAuthor e m => m (Template About)
aboutHandler = baseHandler "About" $ getAbout

editAboutHandler :: (CanAuthor e m, MonadAuth m) => Signed About -> m NoContent
editAboutHandler (Signed sig about@About{..}) = withNamespace "editAbout" $ do
  info $ "request to edit about"
  authorize sig $ aTitle <> aBody
  editAbout about
  pure NoContent

contactHandler :: CanAuthor e m => m (Template Contact)
contactHandler = baseHandler "Contact" getContact

editContactHandler :: (CanAuthor e m, MonadAuth m) => Signed Contact -> m NoContent
editContactHandler (Signed sig contact@Contact{..}) = withNamespace "editContact" $ do
  info $ "request to edit contact"
  authorize sig (l <> e <> li <> fm <> i)
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
  info $ "request for pgp"
  Template "Pgp" <$> view aPgpKey

authorHandler :: (CanAuthor e m, CanAuthEnv a m) => m AuthorTemplate
authorHandler = withNamespace "author" $ do
  info $ "request for author page"
  PgpKey pgpKey <- view aPgpKey
  pure $ AuthorTemplate pgpKey
