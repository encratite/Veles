module Veles.SCGI(
  RequestLengthResult(..),
  determineRequestLength,
  RequestHeader(..),
  parseHeader
  ) where

import Control.Monad
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Map as DM
import Text.Parsec
import Text.Parsec.ByteString

import Knyaz.ByteString
import Knyaz.String

data RequestLengthResult =
  -- the tuple means (length of the SCGI request (without the length string/colon that is), remaining buffer without the length string)
  RegularLengthResult (Maybe (Int, DB.ByteString)) |
  LengthStringConversionError

-- | Try to read the /^\d+:/ part of an SCGI request to its total length.
determineRequestLength :: DB.ByteString -> RequestLengthResult
determineRequestLength buffer =
  case findSubByteString buffer $ DBC.pack ":" of
    Just offset ->
      let lengthString = DBC.unpack $ DB.take offset buffer
          remainingBuffer = DB.drop offset buffer
          lengthMaybe = readMaybe lengthString :: Maybe Int
          in
       case lengthMaybe of
         Just requestLength ->
           -- successfully determined the length of the request
           -- the SCGI netstring size field does not include the size field, its colon nor the terminating comma
           -- add one for the comma:
           RegularLengthResult $ Just (requestLength + 1, remainingBuffer)
         _ ->
           -- the client specified an invalid length string
           LengthStringConversionError
    _ ->
      -- unable to determine the end of the length string (i.e. no colon was found)
      RegularLengthResult Nothing

type RequestHeaderMap = DM.Map String String
data RequestHeader = RequestHeader {
  requestMap :: RequestHeaderMap,
  requestContentLength :: Int
  }

-- | This is the Parsec parser used by 'parseHeader' to parse SCGI headers.
headerParser :: Parser [(String, String)]
headerParser = do
  let stringDelimiter = '\NUL'
      headerDelimiter = ','
      parseString = do
        content <- many1 $ noneOf [stringDelimiter]
        void $ char stringDelimiter
        return content
  pairs <- many . try $ do
    field <- parseString
    value <- parseString
    return (field, value)
  void $ char headerDelimiter
  return pairs

-- | Parses the fields in an SCGI request header and returns them as a map.
-- If it returns Right, the header was parsed successfully, otherwise it returns Left with an error message.
parseHeader :: DB.ByteString -> Either String RequestHeader
parseHeader buffer =
  case parse headerParser "SCGI header" buffer of
    Right pairs ->
      let listPairs = map (:[]) pairs
          multiMap = foldr (++) DM.empty listPairs
          outputMap = DM.map head multiMap
          collisionFields = keys $ filter (\x -> length x > 1) multiMap
          hasCollided = not $ null collisionFields
          lookup = DS.lookup outputMap
          contentLengthField = "CONTENT_LENGTH"
          in
       if hasCollided
       then Left "Invalid SCGI request - detected field collisions in the header: " ++ show collisionFields
       else case lookup "SCGI" of
         Just scgiValue ->
           if scgiValue == "1"
           then case lookup contentLengthField of
             Just contentLengthString ->
               let maybeContentLength = readMaybe contentLengthString :: Maybe Int
               case maybeContentLength of
                 Just contentLength ->
                   RequestHeader outputMap contentLength
                 Nothing ->
                   Left "Invalid content length in SCGI header: " ++ show contentLengthString
             Nothing ->
               Left "The request lacks a " ++ show contentLengthField ++ " field"
           else Left "The SCGI header entry has an invalid value: " ++ show scgiValue
         Nothing ->
           Left "The request lacks an SCGI header entry"
    Left errorMessage ->
      Left "Failed to parse the header of an SCGI request: " ++ errorMessage
