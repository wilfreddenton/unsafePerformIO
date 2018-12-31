{-# LANGUAGE TemplateHaskell #-}

module Lib.Error where

import           Control.Lens        (makeClassyPrisms)
import           Data.Aeson.Extended (ToJSON, Value (Null), encode, object,
                                      toJSON, (.=))
import qualified Data.ByteString     as B
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Lucid.Extended      (HtmlT, Template (Template), ToHtml, h3_,
                                      p_, renderBS, span_, toHtml, toHtmlRaw)
import           Network.HTTP.Types  (Status, hAccept, hContentType, status400,
                                      status404, status500, statusCode,
                                      statusMessage)
import           Network.Wai         (Request, requestHeaders)
import           Protolude
import           Servant             (ServantErr (ServantErr))

class HttpStatus a where
  httpStatus :: a -> Status

class ErrorMessage a where
  errorMessage :: a -> Text

data DbError = DbError1 | DbError2
makeClassyPrisms ''DbError

instance HttpStatus DbError where
  httpStatus _ = status500

instance ErrorMessage DbError where
  errorMessage _ = "undefined"

instance ToJSON DbError where
  toJSON _ = Null

instance ToHtml DbError where
  toHtmlRaw = toHtml
  toHtml = p_ . toHtml . errorMessage

data PostError = PostNotFoundError Text
               | PostPayloadInvalidError
makeClassyPrisms ''PostError

instance HttpStatus PostError where
  httpStatus (PostNotFoundError _)   = status404
  httpStatus PostPayloadInvalidError = status400

instance ErrorMessage PostError where
  errorMessage (PostNotFoundError slug) = "No post is associated with slug: " <> slug
  errorMessage PostPayloadInvalidError = "Post payload was invalid"

instance ToJSON PostError where
  toJSON = toJSON'
    where toJSON' postErr = object [ "code" .= statusCode (httpStatus postErr)
                                   , "message" .= errorMessage postErr
                                   ]

instance ToHtml PostError where
  toHtmlRaw = toHtml
  toHtml = toHtml'
    where
      toHtml' :: Monad m => PostError -> HtmlT m ()
      toHtml' postErr = do
        h3_ $ do
          span_ . toHtml . (<> " "). T.pack . show . statusCode $ httpStatus postErr
          span_ . toHtml . T.decodeUtf8 . statusMessage $ httpStatus postErr
        p_ . toHtml $ errorMessage postErr

data AppError = AppPostError PostError
              | AppDbError DbError
makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsPostError AppError where
  _PostError = _AppPostError . _PostError

instance ToJSON AppError where
  toJSON appErr = case appErr of
    AppPostError err -> toJSON' err
    AppDbError err   -> toJSON' err
    where toJSON' err = object ["error" .= toJSON err]

instance ToHtml AppError where
  toHtmlRaw = toHtml
  toHtml (AppPostError err) = toHtml err
  toHtml (AppDbError err)   = toHtml err

toHttpError :: Request -> AppError -> ServantErr
toHttpError req appErr =
  let headersMap = Map.fromList $ requestHeaders req
      acceptHeaderM = Map.lookup hAccept headersMap
      jsonTuple = (encode . toJSON, (hContentType, "application/json"))
      htmlTuple = (renderBS . toHtml . Template "error", (hContentType, "text/html"))
      (toBS, contentTypeHeader) = case acceptHeaderM of
        Nothing -> jsonTuple
        Just accept  -> if B.isInfixOf "text/html" accept then htmlTuple else jsonTuple
  in ServantErr 500 "Internal Server Error" (toBS appErr) [contentTypeHeader]
