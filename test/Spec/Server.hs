{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Spec.Server (
  serverSpecs
) where

import           Control.Lens       (makeClassy)
import qualified Data.Text          as T
import           Lib.Effects.Auth   (MonadAuth, Signed (Signed), authorize,
                                     authorizePure)
import           Lib.Effects.Logger (MonadLogger, debug, error, info, logPure,
                                     warn, withContext, withContextPure,
                                     withNamespace, withNamespacePure)
import           Lib.Effects.Post   (MonadPost, createPost, createPostSqlite,
                                     deletePost, deletePostSqlite, editPost,
                                     editPostSqlite, getPostById,
                                     getPostByIdSqlite, getPostBySlug,
                                     getPostBySlugSqlite, getPosts,
                                     getPostsSqlite)
import           Lib.Effects.Time   (MonadTime, now, nowIO)
import           Lib.Env            (DbEnv, HasDbEnv (..), newDbEnv)
import           Lib.Error          (AppError (AppPostError),
                                     PostError (PostTitleTooLongError))
import           Lib.Server.Posts   (PostPayload (PostPayload),
                                     createPostHandler)
import           Protolude
import           Test.Tasty         (TestName)
import           Test.Tasty.Hspec   (Spec, describe, it, shouldReturn)

data MockAppEnv = MockAppEnv {
  _mockAppDbEnv :: DbEnv
}
makeClassy ''MockAppEnv

instance HasDbEnv MockAppEnv where
  dbEnv = mockAppDbEnv . dbEnv

newtype MockApp a = MockApp {
  unMockApp :: ReaderT MockAppEnv (ExceptT AppError IO) a
} deriving (Monad, Functor, Applicative, MonadReader MockAppEnv, MonadError AppError, MonadIO)

instance MonadTime MockApp where
  now = nowIO

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
  env <- MockAppEnv <$> newDbEnv "test.db"
  runExceptT . flip runReaderT env $ unMockApp action

serverSpecs :: [(TestName, Spec)]
serverSpecs = [ ("Create Post", createPostSpec)
              -- , ("Edit Post", editPostSpec)
              -- , ("Delete Post", deletePostSpec)
              -- , ("Edit About", editAboutSpec)
              -- , ("Edit Contact", editContactSpec)
              ]

createPostSpec :: Spec
createPostSpec = describe "Create Post Handler" $ do
  it "should return a PostTitleTooLongError" $ do
    runMockApp (createPostHandler $ Signed "" (PostPayload (T.replicate 281 "c") ""))
      `shouldReturn` Left (AppPostError PostTitleTooLongError)

-- editPostSpec :: Spec
-- editPostSpec = undefined

-- deletePostSpec :: Spec
-- deletePostSpec = undefined

-- editAboutSpec :: Spec
-- editAboutSpec = undefined

-- editContactSpec :: Spec
-- editContactSpec = undefined
