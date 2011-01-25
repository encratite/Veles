module Veles.Server(
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
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
runServer port =  withSocketsDo . withLockedConsole $ createServer port

createServer :: Int -> LockedConsole IO ()
createServer port = do
  serverSocket <- liftIO $ socket AF_INET Stream defaultProtocol
  serverAddress <- liftIO $ inet_addr "127.0.0.1"
  liftIO $ do
    bindSocket serverSocket $ SockAddrInet (fromIntegral port) serverAddress
    listen serverSocket 1
  printLine $ "Listening on port " ++ (show port)
  forever $ acceptClient serverSocket

-- | Accept a new client from a server socket and create a new thread to process it.
acceptClient :: Socket -> LockedConsole IO ()
acceptClient serverSocket = do
  printLine "Waiting for a new connection"
  (clientSocket, clientAddress) <- liftIO $ accept serverSocket
  printLine $ "New connection: " ++ (show clientAddress)
  let connectionInformation = ConnectionInformation clientSocket clientAddress
  state <- ask
  void . liftIO . forkIO $ runReaderT (processClient connectionInformation DBC.empty Nothing) state

showByteString :: DB.ByteString -> String
showByteString string = show $ DBC.unpack string

-- | This function specifies the size to be used for each recv call.
receiveSize :: Int
receiveSize = 0x1000

type SCGIRequestLength = Int

-- | Handles the communication with a client in its own thread created by acceptClient.
processClient :: ConnectionInformation -> DB.ByteString -> Maybe SCGIRequestLength -> LockedConsole IO ()
processClient client buffer requestLength = do
  let clientSocket = connectionSocket client
  clientData <- liftIO $ recv clientSocket receiveSize
  let newBuffer = DB.append buffer clientData
      currentLength = DB.length newBuffer

      determineAction expectedLength actionBuffer =
        if currentLength >= expectedLength
        then do
          -- the expected number of bytes has been read, process the fields in the corresponding subset of the buffer
          let bufferOperation f = f expectedLength actionBuffer
              requestBuffer = bufferOperation DBC.take
              remainingBuffer = bufferOperation DBC.drop
          processRequest client requestBuffer
          processClient client remainingBuffer Nothing
        else
          -- still need to read more data - the buffer isn't filled yet
          readMore $ Just expectedLength

      readMore expectedLength =
        processClient client newBuffer expectedLength

  if DB.null clientData
    then printLine $ "Connection closed: " ++ clientAddress
    else do printLine $ "Received " ++ (show $ DB.length clientData) ++ " byte(s) from " ++ clientAddress ++ ": "  ++ showByteString clientData
            case requestLength of
              Just expectedLength ->
                -- the length of the request is already known and does not need to be calculated again
                determineAction expectedLength newBuffer
              _ ->
                -- the length of the request is unknown at this point and is yet to be determined
                let lengthResult = determineRequestLength newBuffer in
                case lengthResult of
                  RegularLengthResult maybeLength ->
                    case maybeLength of
                      Just (expectedLength, remainingBuffer) -> determineAction expectedLength remainingBuffer
                      Nothing -> readMore Nothing
                  LengthStringConversionError ->
                    -- the client has specified an invalid length string, terminate the conection
                    liftIO $ sClose clientSocket
  where
    clientAddress = show $ connectionAddress client

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

-- not implemented yet
processRequest :: ConnectionInformation -> DB.ByteString -> LockedConsole IO ()
processRequest client request = do
  printLine $ "Processing request: " ++ showByteString request