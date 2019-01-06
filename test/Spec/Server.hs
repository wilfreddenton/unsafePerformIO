{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Spec.Server (
  serverSpec
) where

import           Control.Lens       (makeClassy)
import qualified Data.Text          as T
import           Data.Time          (UTCTime (UTCTime), fromGregorian,
                                     secondsToDiffTime)
import           Lib.Effects.Auth   (MonadAuth, Signed (Signed), authorize,
                                     authorizePure)
import           Lib.Effects.Logger (MonadLogger, debug, error, info, logPure,
                                     warn, withContext, withContextPure,
                                     withNamespace, withNamespacePure)
import           Lib.Effects.Post   (MonadPost, Post (Post), createPost,
                                     createPostSqlite, deletePost,
                                     deletePostSqlite, editPost, editPostSqlite,
                                     getPostById, getPostByIdSqlite,
                                     getPostBySlug, getPostBySlugSqlite,
                                     getPosts, getPostsSqlite)
import           Lib.Effects.Time   (MonadTime, now)
import           Lib.Env            (DbEnv, HasDbEnv (..), newDbEnv)
import           Lib.Error          (AppError (AppPostError), PostError (..))
import           Lib.Server.Posts   (PostPayload (PostPayload),
                                     createPostHandler, getPostHandler)
import           Lucid.Extended     (Template (Template))
import           Protolude
import           Servant            (NoContent (NoContent))
import           System.Directory   (removeFile)
import           Test.Tasty.Hspec   (Spec, before_, describe, it, shouldReturn)

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
      postError = Left . AppPostError
      testTitle = "title"
      testBody = "body"
      testSlug = "1994-01-31-" <> testTitle
  describe "Create Post Handler" $ do
    it "returns a PostTitleTooLongError" $
      runCreatePostHandler (T.replicate 281 "c") "" `shouldReturn` postError PostTitleTooLongError
    it "returns a PostTitleEmptyError" $ do
      runCreatePostHandler "" "" `shouldReturn` postError PostTitleEmptyError
    it "returns a PostBodyEmptyError" $ do
      runCreatePostHandler testTitle "" `shouldReturn` postError PostBodyEmptyError
    it "returns NoContent" $ do
      runCreatePostHandler testTitle testBody `shouldReturn` Right NoContent
      runMockApp (getPostHandler testSlug)
        `shouldReturn` Right (Template "Post" $ Post (Just 1) testSlug testTitle testTime testBody)

  describe "Edit Post Handler" $ do
    it "should return a PostBodyEmptyError" $ do
      runMockApp (createPostHandler $ Signed "" (PostPayload "title" ""))
        `shouldReturn` Left (AppPostError PostBodyEmptyError)

-- editPostSpec :: Spec
-- editPostSpec = undefined

-- deletePostSpec :: Spec
-- deletePostSpec = undefined

-- editAboutSpec :: Spec
-- editAboutSpec = undefined

-- editContactSpec :: Spec
-- editContactSpec = undefined
