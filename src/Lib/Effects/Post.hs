{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.Effects.Post where

import           Control.Lens           (view)
import           Data.Aeson.Extended    (ToJSON, genericToJSON, snakeNoPrefix,
                                         toJSON)
import           Data.Char              (isAlphaNum)
import           Data.Map.Strict        as Map
import           Data.Map.Strict        (Map)
import qualified Data.Text              as T
import           Data.Time              (UTCTime (UTCTime), defaultTimeLocale,
                                         formatTime, fromGregorian,
                                         secondsToDiffTime)
import           Database.SQLite.Simple (FromRow, NamedParam ((:=)),
                                         Only (Only), ToRow, execute,
                                         executeNamed, field, fromRow,
                                         queryNamed, query_, toRow)
import           Lib.Db                 (CanDb, liftDbAction)
import           Lib.Effects.Logger     (MonadLogger)
import           Lib.Env                (dConn)
import           Lib.Orphans            ()
import           Lucid.Extended         (HtmlT, ToHtml, class_, colSm4_,
                                         colSm8_, div_, h1_, h3_, href_, li_,
                                         renderMarkdown, row_, span_, termWith,
                                         toHtml, toHtmlRaw, ul_)
import           Protolude

-- Type
data Post = Post {
  pId        :: Maybe Int
, pSlug      :: Text
, pTitle     :: Text
, pCreatedAt :: UTCTime
, pBody      :: Text
} deriving (Eq, Show, Generic)

instance FromRow Post where
  fromRow = Post <$> field <*> field <*> field <*> field <*> field

instance ToRow Post where
  toRow Post {..} = toRow (pSlug, pTitle, pCreatedAt, pBody)

instance ToJSON Post where
  toJSON = genericToJSON snakeNoPrefix

instance ToHtml Post where
  toHtmlRaw = toHtml
  toHtml Post {..} = div_ [class_ "post"] $ do
    h1_ $ toHtml pTitle
    toHtml $ renderMarkdown pTitle pBody

instance ToHtml [Post] where
  toHtmlRaw = toHtml
  toHtml = ul_ . foldMap asListItem
    where
      asListItem :: Monad m => Post -> HtmlT m ()
      asListItem Post {..} = li_ $ do
        termWith "a" [class_ "post-link", href_ $ "/posts/" <> pSlug] $ do
          row_ $ do
            colSm8_ $ do
              h3_ $ do
                span_ [class_ "inset"] "_"
                toHtml $ pTitle
            colSm4_ [class_ "post-list-date"] $ do
              span_ . toHtml . formatTime defaultTimeLocale "%b %d, %_Y" $ pCreatedAt

makeSlug :: Text -> UTCTime -> Text
makeSlug title createdAt = T.pack createdAtStr <> "-" <> modifiedTitle
  where replaceInvalidChar c = if isAlphaNum c then c else ' '
        modifiedTitle = T.intercalate "-" . T.words . T.toLower $ T.map replaceInvalidChar title
        createdAtStr = formatTime defaultTimeLocale "%_Y-%m-%d" createdAt

-- Typeclass
class Monad m => MonadPost m where
  getPosts :: m [Post]
  getPostById :: Int -> m (Maybe Post)
  getPostBySlug :: Text -> m (Maybe Post)
  createPost :: Post -> m ()
  editPost :: Int -> Post -> m ()
  deletePost :: Int -> m ()

-- Implementations

-- SQLite

getPostsSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => m [Post]
getPostsSqlite = do
  conn <- view dConn
  liftDbAction (query_ conn "SELECT * FROM posts ORDER BY created_at DESC" :: IO [Post])

getPostByIdSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Int -> m (Maybe Post)
getPostByIdSqlite id = do
  conn <- view dConn
  posts <- liftDbAction (queryNamed conn "SELECT * FROM posts WHERE id = :id" [":id" := id] :: IO [Post])
  pure $ case posts of
    []  -> Nothing
    p:_ -> Just p

getPostBySlugSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Text -> m (Maybe Post)
getPostBySlugSqlite slug = do
  conn <- view dConn
  posts <- liftDbAction (queryNamed conn "SELECT * FROM posts WHERE slug = :slug" [":slug" := slug] :: IO [Post])
  pure $ case posts of
    []  -> Nothing
    p:_ -> Just p

createPostSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Post -> m ()
createPostSqlite post = do
  conn <- view dConn
  liftDbAction (execute conn "INSERT INTO posts (slug, title, created_at, body) VALUES (?, ?, ?, ?)" post)

editPostSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Int -> Post -> m ()
editPostSqlite id Post{..} = do
  conn <- view dConn
  liftDbAction (executeNamed conn "UPDATE posts SET title = :title, slug = :slug, body = :body WHERE id = :id" params)
  where
    params = [ ":title" := pTitle
             , ":slug" := pSlug
             , ":body" := pBody
             , ":id" := id
             ]

deletePostSqlite :: (MonadLogger m, MonadIO m, CanDb e a m) => Int -> m ()
deletePostSqlite id = do
  conn <- view dConn
  liftDbAction (execute conn "DELETE FROM posts where id = ?" (Only id))

-- Pure

postsMap :: Map Text Post
postsMap = Map.fromList . fmap makePost $ zip (repeat Nothing) postData
  where
    makePost (id, (title, createdAt, body)) =
      let slug = makeSlug title createdAt
      in (slug, Post id slug title createdAt body)
    postData = [
        ("HTML Templating with Lucid", (UTCTime (fromGregorian 2018 12 30) (secondsToDiffTime 0)), "blah blah blah!")
      , ("Haskell JSON Tricks", (UTCTime (fromGregorian 2018 11 20) (secondsToDiffTime 0)), "blah blah blah!")
      , ("Haskell is Frustratingly Good", (UTCTime (fromGregorian 2017 2 12) (secondsToDiffTime 0)), "*unsafePerformIO*")
      , ("Foo", (UTCTime (fromGregorian 2008 8 22) (secondsToDiffTime 0)), "Let's go to the *Bar* ok?")
      , ("Hello, World!", (UTCTime (fromGregorian 1994 1 31) (secondsToDiffTime 0)), "Here is a snippet of code bro.\n```haskell\ninstance ToHtml Post where\n  toHtmlRaw = toHtml\n  toHtml Post{..} = div_ [class_ \"post\"] $ do\n    h1_ $ toHtml title\n    markdown\n    where\n      markdown = toHtml $ case MMark.parse (show title) body of\n        Left _  -> p_ \"invalid markdown\" -- should never run\n        Right m -> MMark.render m\n```\nDid you enjoy this code?")
      ]

getPostsPure :: Monad m => m [Post]
getPostsPure = pure . fmap snd . reverse $ Map.toList postsMap

getPostBySlugPure :: Monad m => Text -> m (Maybe Post)
getPostBySlugPure = pure . flip Map.lookup postsMap
