{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Error where

import           Control.Lens        (makeClassyPrisms)
import           Data.Aeson.Extended (ToJSON, Value, encode, object, toJSON,
                                      (.=))
import qualified Data.ByteString     as B
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Lib.Effects.Logger  (MonadLogger, error, withContext,
                                      withNamespace)
import           Lucid.Extended      (HtmlT, Template (Template), ToHtml, h3_,
                                      p_, renderBS, span_, toHtml, toHtmlRaw)
import           Network.HTTP.Types  (Status (Status), hAccept, hContentType,
                                      status400, status401, status404,
                                      status500, statusCode, statusMessage)
import           Network.Wai         (Request, requestHeaders)
import           Protolude
import           Servant             (ServantErr (ServantErr))

class HttpStatus a where
  httpStatus :: a -> Status

class ErrorMessage a where
  errorMessage :: a -> Text

toJSON' :: (HttpStatus a, ErrorMessage a) => a -> Value
toJSON' a = object [ "code" .= statusCode (httpStatus a)
                   , "message" .= errorMessage a
                   ]

toHtml' :: (HttpStatus a, ErrorMessage a, Monad m) => a -> HtmlT m ()
toHtml' err = do
  h3_ $ do
    span_ . toHtml . (<> " "). T.pack . show $ statusCode status
    span_ . toHtml . T.decodeUtf8 $ statusMessage status
  p_ . toHtml $ errorMessage err
  where status = httpStatus err

data ApiError = NotFoundError Text
              | AuthorizationFailedError Text
              | GetTimeError Text
              | RngError Text
              | FieldTooLongError Text
              | UnauthorizedError
              deriving (Eq, Show)
makeClassyPrisms ''ApiError

instance HttpStatus ApiError where
  httpStatus (NotFoundError _)            = status404
  httpStatus (AuthorizationFailedError _) = status500
  httpStatus (GetTimeError _)             = status500
  httpStatus (RngError _)                 = status500
  httpStatus (FieldTooLongError _)        = status400
  httpStatus UnauthorizedError            = status401

instance ErrorMessage ApiError where
  errorMessage (NotFoundError route)          = "No such route: " <> route
  errorMessage (AuthorizationFailedError err) = "Authorization of request failed with error: " <> err
  errorMessage (GetTimeError err) = "Could not complete request because getting current time failed with error: " <> err
  errorMessage (RngError err) = "Could not complete request because generating random bytes failed with error: " <> err
  errorMessage (FieldTooLongError name) = "Value too long in field: " <> name
  errorMessage UnauthorizedError              = "Unauthorized request"

instance ToJSON ApiError where
  toJSON = toJSON'

instance ToHtml ApiError where
  toHtmlRaw = toHtml
  toHtml = toHtml'

data DbError = DbSqliteError Text | DbError1
  deriving (Eq, Show)
makeClassyPrisms ''DbError

instance HttpStatus DbError where
  httpStatus _ = status500

instance ErrorMessage DbError where
  errorMessage (DbSqliteError msg) = msg
  errorMessage DbError1            = "undefined"

instance ToJSON DbError where
  toJSON = toJSON'

instance ToHtml DbError where
  toHtmlRaw = toHtml
  toHtml = toHtml'

data PostError = PostNotFoundError Text
               | PostTitleTooLongError
               | PostTitleEmptyError
               | PostBodyEmptyError
               deriving (Eq, Show)
makeClassyPrisms ''PostError

instance HttpStatus PostError where
  httpStatus (PostNotFoundError _) = status404
  httpStatus PostTitleTooLongError = status400
  httpStatus PostTitleEmptyError   = status400
  httpStatus PostBodyEmptyError    = status400

instance ErrorMessage PostError where
  errorMessage (PostNotFoundError slug) = "No post is associated with identifier: " <> slug
  errorMessage PostTitleTooLongError = "Post title must be <= 280 characters"
  errorMessage PostTitleEmptyError = "Post title cannot be empty"
  errorMessage PostBodyEmptyError = "Post body cannot be empty"

instance ToJSON PostError where
  toJSON = toJSON'

instance ToHtml PostError where
  toHtmlRaw = toHtml
  toHtml = toHtml'

data AppError = AppApiError ApiError
              | AppPostError PostError
              | AppDbError DbError
              deriving (Eq, Show)
makeClassyPrisms ''AppError

instance AsApiError AppError where
  _ApiError = _AppApiError . _ApiError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsPostError AppError where
  _PostError = _AppPostError . _PostError

instance HttpStatus AppError where
  httpStatus (AppPostError err) = httpStatus err
  httpStatus (AppDbError err)   = httpStatus err
  httpStatus (AppApiError err)  = httpStatus err

instance ErrorMessage AppError where
  errorMessage (AppPostError err) = errorMessage err
  errorMessage (AppDbError err)   = errorMessage err
  errorMessage (AppApiError err)  = errorMessage err

instance ToJSON AppError where
  toJSON appErr = case appErr of
    AppPostError err -> toJSON'' err
    AppDbError err   -> toJSON'' err
    AppApiError err  -> toJSON'' err
    where toJSON'' err = object ["error" .= toJSON err]

instance ToHtml AppError where
  toHtmlRaw = toHtml
  toHtml (AppPostError err) = toHtml err
  toHtml (AppDbError err)   = toHtml err
  toHtml (AppApiError err)  = toHtml err

toHttpError :: Request -> AppError -> ServantErr
toHttpError req appErr =
  let headersMap = Map.fromList $ requestHeaders req
      acceptHeaderM = Map.lookup hAccept headersMap
      Status { statusCode, statusMessage} = httpStatus appErr
      jsonTuple = (encode . toJSON, (hContentType, "application/json"))
      htmlTuple = (renderBS . toHtml . Template "error", (hContentType, "text/html"))
      (toBS, contentTypeHeader) = case acceptHeaderM of
        Nothing -> jsonTuple
        Just accept  -> if B.isInfixOf "text/html" accept then htmlTuple else jsonTuple
  in ServantErr (statusCode) (show $ statusMessage) (toBS appErr) [contentTypeHeader]

logAndThrow :: (MonadLogger m, MonadError e m, ToJSON e) => e -> m a
logAndThrow err = withNamespace "error" . withContext err $ do
  error "request could not be handled due to error"
  throwError err

wrapIO :: (MonadLogger m, MonadError e m, ToJSON e, MonadIO m) => (Text -> e) -> IO a -> m a
wrapIO f action = do
  resultE <- liftIO $ catch (Right <$> action) (\e -> pure . Left . T.pack $ displayException (e :: SomeException))
  case resultE of
    Left err -> logAndThrow $ f err
    Right a  -> pure a

type CanError e m = (MonadError e m, ToJSON e)

type CanApiError e m = (CanError e m, AsApiError e)

type CanPostError e m = (CanError e m, AsPostError e)

type CanDbError e m = (CanError e m, AsDbError e)
