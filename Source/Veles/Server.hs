module Veles.Server(
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import Knyaz.ByteString
import Knyaz.Console
import Knyaz.String

data ConnectionInformation = ConnectionInformation {
  connectionSocket :: Socket,
  connectionAddress :: SockAddr
  }

-- | Run the SCGI server on the specified port.
-- Does not terminate.
runServer :: Int -> IO ()
runServer port =  withSocketsDo . withLockedLinePrinting $ \printLine -> createServer printLine port

createServer :: PrintFunction -> Int -> IO ()
createServer printLine port = do
  serverSocket <- socket AF_INET Stream defaultProtocol
  serverAddress <- inet_addr "127.0.0.1"
  bindSocket serverSocket $ SockAddrInet (fromIntegral port) serverAddress
  listen serverSocket 1
  printLine $ "Listening on port " ++ (show port)
  forever $ acceptClient printLine serverSocket

-- | Accept a new client from a server socket and create a new thread to process it.
acceptClient :: PrintFunction -> Socket -> IO ()
acceptClient printLine serverSocket = do
  printLine "Waiting for a new connection"
  (clientSocket, clientAddress) <- accept serverSocket
  printLine $ "New connection: " ++ (show clientAddress)
  void . forkIO $ processClient printLine (ConnectionInformation clientSocket clientAddress) DBC.empty Nothing
  return ()

-- | This function specifies the size to be used for each recv call.
receiveSize :: Int
receiveSize = 0x1000

type SCGIRequestLength = Int

-- | Handles the communication with a client in its own thread created by acceptClient.
processClient :: PrintFunction -> ConnectionInformation -> DB.ByteString -> Maybe SCGIRequestLength -> IO ()
processClient printLine client buffer requestLength = do
  let clientSocket = connectionSocket client
  clientData <- recv clientSocket receiveSize
  let newBuffer = DB.append buffer clientData
      currentLength = DB.length newBuffer

      determineAction expectedLength =
        if currentLength >= expectedLength
        then
          -- the expected number of bytes has been read, parse the fields in the buffer
          processRequest printLine client newBuffer
        else
          -- still need to read more data - the buffer isn't filled yet
          readMore $ Just expectedLength

      readMore expectedLength =
        processClient printLine client newBuffer expectedLength

  if DB.null clientData
    then printLine $ "Connection closed: " ++ clientAddress
    else do printLine $ "Received " ++ (show $ DB.length clientData) ++ " byte(s) from " ++ clientAddress ++ ": "  ++ (show $ DBC.unpack clientData)
            case requestLength of
              Just expectedLength ->
                -- the length of the request is already known and does not need to be calculated again
                determineAction expectedLength
              _ ->
                -- the length of the request is unknown at this point and is yet to be determined
                let lengthResult = determineRequestLength newBuffer in
                case lengthResult of
                  RegularLengthResult maybeLength ->
                    case maybeLength of
                      Just expectedLength -> determineAction expectedLength
                      Nothing -> readMore Nothing
                  LengthStringConversionError ->
                    -- the client has specified an invalid length string, terminate the conection
                    sClose clientSocket
  where
    clientAddress = show $ connectionAddress client

data RequestLengthResult =
  RegularLengthResult (Maybe Int) |
  LengthStringConversionError

-- | Try to read the /^\d+:/ part of an SCGI request to its total length.
determineRequestLength :: DB.ByteString -> RequestLengthResult
determineRequestLength buffer =
  case findSubByteString buffer $ DBC.pack ":" of
    Just offset ->
      let lengthString = DB.take offset buffer
          lengthMaybe = readMaybe (DBC.unpack lengthString) :: Maybe Int
          in
       case lengthMaybe of
         Just _ ->
           -- successfully determined the length of the request
           RegularLengthResult lengthMaybe
         _ ->
           -- the client specified an invalid length string
           LengthStringConversionError
    _ ->
      -- unable to determine the end of the length string (i.e. no colon was found)
      RegularLengthResult Nothing

-- not implemented yet
processRequest :: PrintFunction -> ConnectionInformation -> DB.ByteString -> IO ()
processRequest printLine client buffer = return ()