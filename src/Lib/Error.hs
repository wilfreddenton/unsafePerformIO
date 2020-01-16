{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Error where

import Control.Lens ((#), Prism', makeClassyPrisms)
import Data.Aeson.Extended ((.=), ToJSON, Value, object, toJSON)
import Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Validation (Validation (..))
import Lib.Effects.Logger
  ( MonadLogger,
    error,
    withContext,
    withNamespace,
  )
import Lucid.Extended
  ( HtmlT,
    ToHtml,
    h3_,
    p_,
    span_,
    toHtml,
    toHtmlRaw,
  )
import Network.HTTP.Types
  ( Status,
    status400,
    status401,
    status404,
    status500,
    statusCode,
    statusMessage,
  )
import Protolude

class HttpStatus a where
  httpStatus :: a -> Status

class ErrorMessage a where
  errorMessage :: a -> Text

toJSON' :: (HttpStatus a, ErrorMessage a) => a -> Value
toJSON' a =
  object
    [ "code" .= statusCode (httpStatus a),
      "message" .= errorMessage a
    ]

toHtml' :: (HttpStatus a, ErrorMessage a, Monad m) => a -> HtmlT m ()
toHtml' err = do
  h3_ $ do
    span_ . toHtml . (<> " ") . T.pack . show $ statusCode status
    span_ . toHtml . T.decodeUtf8 $ statusMessage status
  p_ . toHtml $ errorMessage err
  where
    status = httpStatus err

data ApiError
  = NotFoundError Text
  | AuthorizationFailedError Text
  | GetTimeError Text
  | RngError Text
  | UnauthorizedError
  deriving (Eq, Show)

makeClassyPrisms ''ApiError

instance HttpStatus ApiError where
  httpStatus (NotFoundError _) = status404
  httpStatus (AuthorizationFailedError _) = status500
  httpStatus (GetTimeError _) = status500
  httpStatus (RngError _) = status500
  httpStatus UnauthorizedError = status401

instance ErrorMessage ApiError where
  errorMessage (NotFoundError route) = "No such route: " <> route
  errorMessage (AuthorizationFailedError err) = "Authorization of request failed with error: " <> err
  errorMessage (GetTimeError err) = "Could not complete request because getting current time failed with error: " <> err
  errorMessage (RngError err) = "Could not complete request because generating random bytes failed with error: " <> err
  errorMessage UnauthorizedError = "Unauthorized request"

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
  errorMessage DbError1 = "undefined"

instance ToJSON DbError where
  toJSON = toJSON'

instance ToHtml DbError where

  toHtmlRaw = toHtml

  toHtml = toHtml'

data AuthorError
  = AboutValidationError Text
  | ContactValidationError Text
  deriving (Eq, Show)

makeClassyPrisms ''AuthorError

instance HttpStatus AuthorError where
  httpStatus _ = status400

instance ErrorMessage AuthorError where
  errorMessage (AboutValidationError err) = "Submitted about info invalid: \n" <> err
  errorMessage (ContactValidationError err) = "Submitted contact info invalid: \n" <> err

instance ToJSON AuthorError where
  toJSON = toJSON'

instance ToHtml AuthorError where

  toHtmlRaw = toHtml

  toHtml = toHtml'

data PostError
  = PostNotFoundError Text
  | PostValidationError Text
  deriving (Eq, Show)

makeClassyPrisms ''PostError

instance HttpStatus PostError where
  httpStatus (PostNotFoundError _) = status404
  httpStatus (PostValidationError _) = status400

instance ErrorMessage PostError where
  errorMessage (PostNotFoundError slug) = "No post is associated with identifier: " <> slug
  errorMessage (PostValidationError err) = "Submitted post data invalid: \n" <> err

instance ToJSON PostError where
  toJSON = toJSON'

instance ToHtml PostError where

  toHtmlRaw = toHtml

  toHtml = toHtml'

data AppError
  = AppApiError ApiError
  | AppAuthorError AuthorError
  | AppPostError PostError
  | AppDbError DbError
  deriving (Eq, Show)

makeClassyPrisms ''AppError

instance AsApiError AppError where
  _ApiError = _AppApiError . _ApiError

instance AsAuthorError AppError where
  _AuthorError = _AppAuthorError . _AuthorError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsPostError AppError where
  _PostError = _AppPostError . _PostError

instance HttpStatus AppError where
  httpStatus (AppApiError err) = httpStatus err
  httpStatus (AppAuthorError err) = httpStatus err
  httpStatus (AppPostError err) = httpStatus err
  httpStatus (AppDbError err) = httpStatus err

instance ErrorMessage AppError where
  errorMessage (AppApiError err) = errorMessage err
  errorMessage (AppAuthorError err) = errorMessage err
  errorMessage (AppPostError err) = errorMessage err
  errorMessage (AppDbError err) = errorMessage err

instance ToJSON AppError where
  toJSON appErr = case appErr of
    AppApiError err -> toJSON'' err
    AppAuthorError err -> toJSON'' err
    AppPostError err -> toJSON'' err
    AppDbError err -> toJSON'' err
    where
      toJSON'' err = object ["error" .= toJSON err]

instance ToHtml AppError where

  toHtmlRaw = toHtml

  toHtml (AppApiError err) = toHtml err
  toHtml (AppAuthorError err) = toHtml err
  toHtml (AppPostError err) = toHtml err
  toHtml (AppDbError err) = toHtml err

logAndThrow :: (MonadLogger m, MonadError e m, ToJSON e) => e -> m a
logAndThrow err = withNamespace "error" . withContext err $ do
  error "request could not be handled due to error"
  throwError err

wrapIO :: (MonadLogger m, MonadError e m, ToJSON e, MonadIO m) => (Text -> e) -> IO a -> m a
wrapIO f action = do
  resultE <- liftIO $ catch (Right <$> action) (\e -> pure . Left . T.pack $ displayException (e :: SomeException))
  case resultE of
    Left err -> logAndThrow $ f err
    Right a -> pure a

type CanError e m = (MonadError e m, ToJSON e)

type CanApiError e m = (CanError e m, AsApiError e)

type CanAuthorError e m = (CanError e m, AsAuthorError e)

type CanPostError e m = (CanError e m, AsPostError e)

type CanDbError e m = (CanError e m, AsDbError e)

type AppValidation = Validation (NonEmpty Text) ()

failure :: Text -> AppValidation
failure = Failure . flip (:|) []

validateMinLength :: (Coercible a Text) => Int -> Text -> a -> AppValidation
validateMinLength lo fieldName field =
  if lo <= T.length (coerce field)
    then Success ()
    else failure $ "Field '" <> fieldName <> "' must " <> msgPart
  where
    msgPart = case lo of
      1 -> "not be empty."
      _ -> "be at least " <> show lo <> " characters long."

validateMaxLength :: (Coercible a Text) => Int -> Text -> a -> AppValidation
validateMaxLength hi fieldName field =
  if T.length (coerce field) <= hi
    then Success ()
    else failure $ "Field '" <> fieldName <> "' can be at most " <> show hi <> " characters long."

validateLength :: (Coercible a Text) => Int -> Int -> Text -> a -> AppValidation
validateLength lo hi fieldName field =
  validateMinLength lo fieldName field <* validateMaxLength hi fieldName field

throwInvalid :: (MonadLogger m, CanError e m) => Prism' e Text -> AppValidation -> m ()
throwInvalid _ (Success _) = pure ()
throwInvalid mkError (Failure errs) = logAndThrow $ mkError # (T.strip . T.unlines $ NE.toList errs)
