module Request(
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

type ContentProcessingResult = DB.ByteString -> Either String RequestContent

-- | Attempts to fully decode the RFC 1738 URL encoded content of a request.
processURLEncodedData :: ContentProcessingResult
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

parseHTTPHeaderValue :: Parser [HTTPHeaderValueField]
parseHTTPHeaderValue = do
  fields <- many1 $ do
    skipMany $ char space
    field <- valuePair <|> valueField
    return field
  where
    separator = ';'
    space = ' '
    fieldSeparator = '='

    endParser = (void $ char separator) <|> eof
    contentParser = many1 $ noneOf [separator, fieldSeparator]
    rightTrim = reverse . dropWhile (== space) . reverse

    valuePair :: Parser (HTTPHeaderValuePair (String, String))
    valuePair = do
      field <- contentParser
      void $ char fieldSeparator
      untrimmedValue <- contentParser
      let value = rightTrim untrimmedValue
      endParser
      return $ HTTPHeaderValuePair field value

    valueField :: Parser (HTTPHeaderValueField String)
    valueField = do
      untrimmedValue <- contentParser
      let value = rightTrim untrimmedValue
      endParser
      return $ HTTPHeaderValueField value

processMultipartEncodedData :: ContentProcessingResult
processMultipartEncodedData contentBuffer =


-- | Attempts to decode the content of a request to insert the decoded pairs into a map.
parseRequestContent :: RequestHeaderMap -> DB.ByteString -> Either String RequestContent
parseRequestContent headerMap contentBuffer =
  case DM.lookup "Content-Type" headerMap of
    Just contentType ->
      case contentType of
        "application/x-www-form-urlencoded" ->
          processURLEncodedData contentBuffer
        _ ->
          let multipartIdentifier = "multipart/form-data"
              in
           case parse parseHTTPHeaderValue "HTTP header value" contentType of
             Left errorMessage ->
               Left $ "Unable to parse the content type of a request: " ++ errorMessage
             Right fields ->
               Left "Encountered an unknown content type in a request: " ++ consoleString contentType
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