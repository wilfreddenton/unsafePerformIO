{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Spec.Server (
  serverSpec
) where

import           Control.Lens        (makeClassy)
import           Data.Aeson.Extended (Value (Null))
import qualified Data.Text           as T
import           Data.Time           (UTCTime (UTCTime), fromGregorian,
                                      secondsToDiffTime)
import           Lib.Effects.Auth    (MonadAuth, Signed (Signed), authorize,
                                      authorizePure)
import           Lib.Effects.Logger  (MonadLogger, debug, error, info, logPure,
                                      warn, withContext, withContextPure,
                                      withNamespace, withNamespacePure)
import           Lib.Effects.Post    (MonadPost, Post (Post), createPost,
                                      createPostSqlite, deletePost,
                                      deletePostSqlite, editPost,
                                      editPostSqlite, getPostById,
                                      getPostByIdSqlite, getPostBySlug,
                                      getPostBySlugSqlite, getPosts,
                                      getPostsSqlite, makeSlug)
import           Lib.Effects.Time    (MonadTime, now)
import           Lib.Env             (DbEnv, HasDbEnv (..), newDbEnv)
import           Lib.Error           (AppError (AppPostError), PostError (..))
import           Lib.Server.Posts    (PostPayload (PostPayload),
                                      createPostHandler, deletePostHandler,
                                      editPostHandler, getPostHandler)
import           Lucid.Extended      (Template (Template))
import           Protolude
import           Servant             (NoContent (NoContent))
import           System.Directory    (removeFile)
import           Test.Tasty.Hspec    (Spec, before_, describe, it, shouldReturn)

data MockAppEnv = MockAppEnv {
  _mockAppDbEnv :: DbEnv
}
makeClassy ''MockAppEnv

instance HasDbEnv MockAppEnv where
  dbEnv = mockAppDbEnv . dbEnv

newtype MockApp a = MockApp {
  unMockApp :: ReaderT MockAppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader MockAppEnv, MonadError AppError, MonadIO)

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

runMockApp :: MockApp a -> IO (Either AppError a)
runMockApp action = do
  env <- MockAppEnv <$> newDbEnv "test.db" "init.sql"
  runExceptT . flip runReaderT env $ unMockApp action

resetDb :: IO ()
resetDb = removeFile "test.db"

serverSpec :: Spec
serverSpec = before_ resetDb $ do
  let runCreatePostHandler title body = runMockApp (createPostHandler $ Signed "" (PostPayload title body))
      runGetPostHandler slug = runMockApp (getPostHandler slug)
      postError = Left . AppPostError
      postSuccess name = Right . Template name
      testInvalidTitle = T.replicate 281 "c"
      testTitle = "title"
      testBody = "body"
      testSlug = makeSlug testTitle testTime
  describe "Create Post Handler" $ do
    it "returns a PostTitleTooLongError" $
      runCreatePostHandler testInvalidTitle "" `shouldReturn` postError PostTitleTooLongError
    it "returns a PostTitleEmptyError" $
      runCreatePostHandler "" "" `shouldReturn` postError PostTitleEmptyError
    it "returns a PostBodyEmptyError" $
      runCreatePostHandler testTitle "" `shouldReturn` postError PostBodyEmptyError
    it "successfully creates a Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runGetPostHandler testSlug
        `shouldReturn` postSuccess "Post" (Post (Just 1) testSlug testTitle testTime testBody)

  let runEditPostHandler id title body = runMockApp (editPostHandler id $ Signed "" (PostPayload title body))
      newTestTitle = "new title"
      newTestBody = "new body"
      newTestSlug = makeSlug newTestTitle testTime
  describe "Edit Post Handler" $ do
    it "returns a PostTitleTooLongError" $
      runEditPostHandler 1 testInvalidTitle "" `shouldReturn` postError PostTitleTooLongError
    it "returns a PostTitleEmptyError" $
      runEditPostHandler 1 "" "" `shouldReturn` postError PostTitleEmptyError
    it "returns a PostBodyEmptyError" $
      runEditPostHandler 1 testTitle "" `shouldReturn` postError PostBodyEmptyError
    it "returns a PostNotFoundError" $
      runEditPostHandler 1 newTestTitle newTestBody `shouldReturn` postError (PostNotFoundError "1")
    it "successfully edits a Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runEditPostHandler 1 newTestTitle newTestBody `shouldReturn` Right NoContent
      runGetPostHandler newTestSlug
        `shouldReturn` postSuccess "Post" (Post (Just 1) newTestSlug newTestTitle testTime newTestBody)

  let runDeletePostHandler id = runMockApp (deletePostHandler id $ Signed "" Null)
  describe "Delete Post Handler" $ do
    it "returns a PostNotFoundError" $
      runDeletePostHandler 1 `shouldReturn` postError (PostNotFoundError "1")
    it "successfully deletes Post" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runDeletePostHandler 1 `shouldReturn` Right NoContent
      runGetPostHandler newTestSlug
        `shouldReturn` postError (PostNotFoundError newTestSlug)

-- deletePostSpec :: Spec
-- deletePostSpec = undefined

-- editAboutSpec :: Spec
-- editAboutSpec = undefined

-- editContactSpec :: Spec
-- editContactSpec = undefined
