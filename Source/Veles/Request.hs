module Request(
  RequestMethod,
  RequestBody,
  Request(..)
  ) where

import qualified Data.ByteString as DB
import qualified Data.Map as DM

data RequestMethod = GetMethod | PostMethod deriving Show

type RequestContent = DM.Map String DB.ByteString

data Request = Request {
  requestPath :: String,
  requestMethod :: RequestMethod,
  requestContent :: RequestContent
  }