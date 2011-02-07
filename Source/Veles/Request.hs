Request module(
  RequestMethod,
  RequestBody,
  Request(..)
  ) where

import Control.Monad
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Map as DM
import Text.Parsec
import Text.Parsec.ByteString
import Web.Encodings

import Knyaz.String

import Veles.Console
import Veles.SCGI

data RequestMethod = GetMethod | PostMethod deriving Show

type RequestContent = DM.Map String DB.ByteString

data Request = Request {
  requestPath :: String,
  requestMethod :: RequestMethod,
  requestContent :: RequestContent
  }

-- | Retrieves the tokens of an RFC 1738 URL encoded string without decoding the inner content yet.
urlEncodingTokenParser :: Parser [(String, String)]
urlEncodingTokenParser = do
  let fieldValueSeparator = '='
      entrySeparator = '&'
      validChars = letter <|> digit <|> oneOf "-_.~"
  pairs <- many $ do
    fieldName <- many1 validChars
    void $ char fieldValueSeparator
    value <- many validChars
    void $ entrySeparator <|> eof
    return (fieldName, value)
  return pairs

type ContentProcessor = DB.ByteString -> Either String RequestContent

-- | Attempts to fully decode the RFC 1738 URL encoded content of a request.
processURLEncodedData :: ContentProcessor
processURLEncodedData contentBuffer =
  case parse urlEncodingTokenParser "URL encoding tokens" contentBuffer of
    Left errorMessage ->
      Left "Failed to decode the tokens in the URL encoded content of a request: " ++ errorMessage
    Right pairs ->
      let decodedPairs = map decoder pairs in
      DM.fromList decodedPairs
  where
    decoder (field, value) =
      let f = decodeUrl in
      (f field, f value)

data HTTPHeaderValueField =
  HTTPHeaderValueField String |
  HTTPHeaderValuePair (String, String)

parseHTTPHeaderValue :: String -> Either String [HTTPHeaderValueField]
parseHTTPHeaderValue string =
   if hasEmptyToken
   then Left "Encountered an empty token"
   else if invalidTokenCount
        then Left "Invalid token count"
        else Right valueFields
  where
    groups = tokeniseString string ";"
    tokenGroups = map (\x -> tokeniseString x "=") groups
    hasEmptyToken = any (\x -> any null x) tokenGroups
    invalidTokenCount = any (== Nothing) maybeFields
    maybeFields = map maybeField tokenGroups
    maybeField tokens =
      case tokens of
        (field : value : []) ->
          Just $ HTTPHeaderValuePair (field, value)
        (value : []) ->
          Just $ HTTPHeaderValueField value
        _ ->
          Nothing

processMultipartEncodedData :: String -> ContentProcessor
processMultipartEncodedData boundary contentBuffer = undefined

-- | Attempts to decode the content of a request to insert the decoded pairs into a map.
parseRequestContent :: RequestHeaderMap -> DB.ByteString -> Either String RequestContent
parseRequestContent headerMap contentBuffer =
  let urlEncodedIdentifier = "application/x-www-form-urlencoded"
      multipartIdentifier = "multipart/form-data"
      in
  case DM.lookup "Content-Type" headerMap of
    Just contentType ->
      case contentType of
        urlEncodedIdentifier ->
          processURLEncodedData contentBuffer
        _ ->
           case parseHTTPHeaderValue contentType of
             Left errorMessage ->
               Left $ "Unable to parse the content type of a request: " ++ errorMessage
             Right fields ->
               if length fields == 2 && head fields == HTTPHeaderValueField multipartIdentifier
               then
                 case last fields of
                   HTTPHeaderValuePair ("boundary", boundary) ->
                     Right $ processMultipartEncodedData boundary contentBuffer
                   _ ->
                     Left $ "Invalid multipart boundary in content type: " ++ consoleString contentType
               else
                 Left $ "Encountered an unknown content type in a request: " ++ consoleString contentType
    Nothing ->
      if DB.null contentBuffer
      then DM.empty
      else Left "The request has no content type specified, yet it features content"

-- | Attempts to create a 'Request' which can actually be used by the request handlers outside this framework based on the request header and the request content.
parseRequest :: RequestHeader -> DB.ByteString -> Either String Request
parseRequest header contentBuffer =
  let path = requestHeaderURI header
      method = requestHeaderMethod header
      contentResult = parseRequestContent (requestHeaderMap header) contentBuffer
      in
   case contentResult of
     Left error ->
       Left error
     Right content ->
       Request path method content