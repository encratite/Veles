module Veles.SCGI(
  RequestLengthResult(RegularLengthResult, LengthStringConversionError),
  determineRequestLength
  ) where

import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC

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
           -- this length does not include the length string and the colon itself, though, so the buffer should be adjusted accordingly
           RegularLengthResult $ Just (requestLength, remainingBuffer)
         _ ->
           -- the client specified an invalid length string
           LengthStringConversionError
    _ ->
      -- unable to determine the end of the length string (i.e. no colon was found)
      RegularLengthResult Nothing