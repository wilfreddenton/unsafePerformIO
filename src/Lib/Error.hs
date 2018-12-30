module Lib.Error where

import           Data.Aeson.Extended (ToJSON, encode, object, toJSON, (.=))
import qualified Data.ByteString     as B
import qualified Data.Map.Strict     as Map
import           Lucid.Extended      (Template (Template), ToHtml, p_, renderBS,
                                      toHtml, toHtmlRaw)
import           Network.HTTP.Types  (hAccept, hContentType)
import           Network.Wai         (Request, requestHeaders)
import           Protolude
import           Servant             (ServantErr (ServantErr))

data AppError = AppError Text

instance ToJSON AppError where
  toJSON (AppError err) = object ["error" .= err]

instance ToHtml AppError where
  toHtmlRaw = toHtml
  toHtml (AppError err) = p_ $ toHtml err

toHttpError :: Request -> AppError -> ServantErr
toHttpError req appErr =
  let headersMap = Map.fromList $ requestHeaders req
      acceptHeaderM = Map.lookup hAccept headersMap
      jsonTuple = (encode . toJSON, (hContentType, "application/json"))
      htmlTuple = (renderBS . toHtml . Template "error", (hContentType, "text/html"))
      (toBS, contentType) = case acceptHeaderM of
        Nothing -> jsonTuple
        Just accept  -> if B.isInfixOf "text/html" accept then htmlTuple else jsonTuple
  in ServantErr 500 "Internal Server Error" (toBS appErr) [contentType]
