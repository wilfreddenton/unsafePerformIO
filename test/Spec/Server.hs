{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Spec.Server
  ( serverSpec,
  )
where

import Control.Lens (makeClassy)
import Data.Aeson.Extended (Value (Null))
import qualified Data.Text as T
import Data.Time
  ( UTCTime (UTCTime),
    fromGregorian,
    secondsToDiffTime,
  )
import Lib.Effects.Auth
  ( MonadAuth,
    Signed (Signed),
    authorize,
    authorizePure,
  )
import Lib.Effects.Author
  ( About (About),
    Contact (..),
    Email (Email),
    FacebookMessenger (FacebookMessenger),
    Instagram (Instagram),
    LinkedIn (LinkedIn),
    MonadAuthor,
    MyLocation (MyLocation),
    editAbout,
    editAboutSqlite,
    editContact,
    editContactSqlite,
    getAbout,
    getAboutSqlite,
    getContact,
    getContactSqlite,
  )
import Lib.Effects.Logger
  ( MonadLogger,
    debug,
    error,
    info,
    logPure,
    warn,
    withContext,
    withContextPure,
    withNamespace,
    withNamespacePure,
  )
import Lib.Effects.Post
  ( MonadPost,
    Post (Post),
    createPost,
    createPostSqlite,
    deletePost,
    deletePostSqlite,
    editPost,
    editPostSqlite,
    getPostById,
    getPostByIdSqlite,
    getPostBySlug,
    getPostBySlugSqlite,
    getPosts,
    getPostsSqlite,
    makeSlug,
  )
import Lib.Effects.Time (MonadTime, now)
import Lib.Env (DbEnv, HasDbEnv (..), newDbEnv)
import Lib.Error
  ( ApiError (NotFoundError),
    AppError (AppApiError, AppAuthorError, AppPostError),
    AuthorError (AboutValidationError, ContactValidationError),
    PostError (..),
  )
import Lib.Server.Author
  ( editAboutHandler,
    editContactHandler,
    getAboutHandler,
    getContactHandler,
  )
import Lib.Server.Posts
  ( PostPayload (PostPayload),
    createPostHandler,
    deletePostHandler,
    editPostHandler,
    getPostHandler,
  )
import Lib.Server.Template (Template (Template))
import Protolude
import Servant (NoContent (NoContent))
import System.Directory (doesFileExist, removeFile)
import Test.Tasty.Hspec (Spec, before_, describe, it, shouldReturn)

data MockAppEnv
  = MockAppEnv
      { _mockAppDbEnv :: DbEnv
      }

makeClassy ''MockAppEnv

instance HasDbEnv MockAppEnv where
  dbEnv = mockAppDbEnv . dbEnv

newtype MockApp a
  = MockApp
      { unMockApp :: ReaderT MockAppEnv (ExceptT AppError IO) a
      }
  deriving (Monad, Functor, Applicative, MonadReader MockAppEnv, MonadError AppError, MonadIO)

testTime :: UTCTime
testTime = UTCTime (fromGregorian 1994 1 31) $ secondsToDiffTime 0

instance MonadTime MockApp where
  now = pure testTime

instance MonadAuth MockApp where
  authorize = authorizePure

instance MonadLogger MockApp where

  debug = logPure

  error = logPure

  info = logPure

  warn = logPure

  withNamespace = withNamespacePure

  withContext = withContextPure

instance MonadPost MockApp where

  getPosts = getPostsSqlite

  getPostById = getPostByIdSqlite

  getPostBySlug = getPostBySlugSqlite

  createPost = createPostSqlite

  editPost = editPostSqlite

  deletePost = deletePostSqlite

instance MonadAuthor MockApp where

  getAbout = getAboutSqlite

  editAbout = editAboutSqlite

  getContact = getContactSqlite

  editContact = editContactSqlite

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  env <- MockAppEnv <$> newDbEnv "test.db" "init.sql"
  runExceptT . flip runReaderT env $ unMockApp action

resetDb :: IO ()
resetDb = do
  b <- doesFileExist file
  if b then removeFile file else pure ()
  where
    file = "test.db"

serverSpec :: Spec
serverSpec = before_ resetDb $ do
  let runCreatePostHandler title body = runMockApp . createPostHandler . Signed "" $ PostPayload title body
      runGetPostHandler = runMockApp . getPostHandler
      postError = Left . AppPostError
      authorError = Left . AppAuthorError
      postSuccess name = Right . Template name
      testInvalidTitle = T.replicate 281 "c"
      testTitle = "title"
      testBody = "body"
      testSlug = makeSlug testTitle testTime
  describe "Create Post Handler" $ do
    it "returns PostValidationError when title is too long" $
      runCreatePostHandler testInvalidTitle "foo"
        `shouldReturn` postError (PostValidationError "Field 'title' can be at most 280 characters long.")
    it "returns PostValidationError when title is empty" $
      runCreatePostHandler "" "foo"
        `shouldReturn` postError (PostValidationError "Field 'title' must be at least 3 characters long.")
    it "returns PostValidationError when body is empty" $
      runCreatePostHandler testTitle ""
        `shouldReturn` postError (PostValidationError "Field 'body' must not be empty.")
    it "returns PostValidationError when title and body are empty" $
      runCreatePostHandler "" ""
        `shouldReturn` postError
          ( PostValidationError . T.strip $
              T.unlines
                [ "Field 'title' must be at least 3 characters long.",
                  "Field 'body' must not be empty."
                ]
          )
    it "creates Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runGetPostHandler testSlug
        `shouldReturn` postSuccess testTitle (Post (Just 1) testSlug testTitle testTime testBody)
  let runEditPostHandler id title body =
        runMockApp . editPostHandler id . Signed "" $ PostPayload title body
      newTestTitle = "new title"
      newTestBody = "new body"
      newTestSlug = makeSlug newTestTitle testTime
  describe "Edit Post Handler" $ do
    it "returns PostValidationError when title is too long" $
      runEditPostHandler 1 testInvalidTitle "foo"
        `shouldReturn` postError (PostValidationError "Field 'title' can be at most 280 characters long.")
    it "returns PostValidationError when title is empty" $
      runEditPostHandler 1 "" "foo"
        `shouldReturn` postError (PostValidationError "Field 'title' must be at least 3 characters long.")
    it "returns PostValidationError when body is empty" $
      runEditPostHandler 1 testTitle ""
        `shouldReturn` postError (PostValidationError "Field 'body' must not be empty.")
    it "returns PostValidationError when title and body are empty" $
      runEditPostHandler 1 "" ""
        `shouldReturn` postError
          ( PostValidationError . T.strip $
              T.unlines
                [ "Field 'title' must be at least 3 characters long.",
                  "Field 'body' must not be empty."
                ]
          )
    it "returns PostNotFoundError" $
      runEditPostHandler 1 newTestTitle newTestBody `shouldReturn` postError (PostNotFoundError "1")
    it "edits Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runEditPostHandler 1 newTestTitle newTestBody `shouldReturn` Right NoContent
      runGetPostHandler newTestSlug
        `shouldReturn` postSuccess newTestTitle (Post (Just 1) newTestSlug newTestTitle testTime newTestBody)
  let runDeletePostHandler id = runMockApp . deletePostHandler id $ Signed "" Null
  describe "Delete Post Handler" $ do
    it "returns PostNotFoundError" $
      runDeletePostHandler 1 `shouldReturn` postError (PostNotFoundError "1")
    it "deletes Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runDeletePostHandler 1 `shouldReturn` Right NoContent
      runGetPostHandler newTestSlug
        `shouldReturn` postError (PostNotFoundError newTestSlug)
  let runGetAboutHandler = runMockApp getAboutHandler
      apiError = Left . AppApiError
  describe "Get About Handler"
    $ it "returns NotFoundError"
    $ runGetAboutHandler `shouldReturn` apiError (NotFoundError "about")
  let runEditAboutHandler about = runMockApp . editAboutHandler $ Signed "" about
      testAbout = About "title" "body"
      newTestAbout = About "newtitle" "newbody"
      aboutSucces = Right . Template "About"
  describe "Edit About Handler" $ do
    it "returns AboutValidationError" $
      runEditAboutHandler (About testInvalidTitle "")
        `shouldReturn` authorError
          ( AboutValidationError
              "Field 'title' can be at most 280 characters long."
          )
  it "creates and edits About" $ do
    runEditAboutHandler testAbout `shouldReturn` Right NoContent
    runGetAboutHandler `shouldReturn` aboutSucces testAbout
    runEditAboutHandler newTestAbout `shouldReturn` Right NoContent
    runGetAboutHandler `shouldReturn` aboutSucces newTestAbout
  let runGetContactHandler = runMockApp getContactHandler
      newContact l e li f i = Contact (MyLocation l) (Email e) (LinkedIn li) (FacebookMessenger f) (Instagram i)
      testContact = newContact "location" "email" "linked_in" "facebook_messenger" "instagram"
      contactSuccess = Right . Template "Contact"
  describe "Get Contact Handler"
    $ it "returns NotFoundError"
    $ runGetContactHandler `shouldReturn` apiError (NotFoundError "contact")
  let runEditContactHandler contact = runMockApp . editContactHandler $ Signed "" contact
      newTestContactE = Email "new_email"
      newTestContactFm = FacebookMessenger "new_facebook_messenger"
      newTestContact = testContact {cEmail = newTestContactE, cFacebookMessenger = newTestContactFm}
  describe "Edit Contact Handler" $ do
    it "returns ContactValidationError" $ do
      runEditContactHandler (newContact "" testInvalidTitle "" "" "")
        `shouldReturn` authorError
          ( ContactValidationError . T.strip $
              T.unlines
                [ "Field 'location' must be at least 3 characters long.",
                  "Field 'email' can be at most 280 characters long.",
                  "Field 'LinkedIn' must be at least 3 characters long.",
                  "Field 'Facebook Messenger' must be at least 3 characters long.",
                  "Field 'Instagram' must be at least 3 characters long."
                ]
          )
    it "creates and edits Contact" $ do
      runEditContactHandler testContact `shouldReturn` Right NoContent
      runGetContactHandler `shouldReturn` contactSuccess testContact
      runEditContactHandler newTestContact `shouldReturn` Right NoContent
      runGetContactHandler `shouldReturn` contactSuccess newTestContact
