module Lib.Error where

import           Data.Aeson (encode)
import           Protolude
import           Servant    (ServantErr (ServantErr))

data AppError = AppError Text

toHttpError :: AppError -> ServantErr
toHttpError (AppError err) = ServantErr 500 "Internal Server Error" (encode err) []
