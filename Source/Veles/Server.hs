module Veles.Server(
  ConnectionInformation(..),
  runServer
  ) where

import Control.Monad
import Control.Monad.State -- temporary
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import Knyaz.Console
import Knyaz.Reader

import Veles.ConnectionInformation
import Veles.Client
import Veles.SCGI

-- | Run the SCGI server on the specified port.
-- Does not terminate.
runServer :: Int -> IO ()
runServer port =  withSocketsDo . withLockedConsole $ createServer port

-- | Create a new SCGI server on the specified port and start accepting clients.
-- Does not terminate.
createServer :: Int -> LockedConsoleT IO ()
createServer port = do
  serverSocket <- liftIO $ socket AF_INET Stream defaultProtocol
  serverAddress <- liftIO $ inet_addr "127.0.0.1"
  liftIO $ do
    bindSocket serverSocket $ SockAddrInet (fromIntegral port) serverAddress
    listen serverSocket 1
  printLine $ "Listening on port " ++ (show port)
  forever $ acceptClient serverSocket

-- | Accept a new client from a server socket and create a new thread to process it.
acceptClient :: Socket -> LockedConsoleT IO ()
acceptClient serverSocket = do
  printLine "Waiting for a new connection"
  (clientSocket, clientAddress) <- liftIO $ accept serverSocket
  printLine $ "New connection: " ++ (show clientAddress)
  let connectionInformation = ConnectionInformation clientSocket clientAddress
  forkReader . void $ withClientEnvironment connectionInformation $ readClientData headerLengthReader

-- | Unpack and show a ByteString so it can be printed to stdio.
showByteString :: DB.ByteString -> String
showByteString string = show $ DBC.unpack string

-- | This function specifies the size to be used for each recv call.
receiveSize :: Int
receiveSize = 0x1000

type ClientEnvironment = ClientEnvironmentT IO ()

-- | Read data from a client and pass on the environment to the specified handler to process what has been received.
readClientData :: ClientEnvironment -> ClientEnvironment
readClientData handler = do
  clientSocket <- getSocket
  newData <- liftIO $ recv clientSocket receiveSize
  if DB.null newData
    then clientPrint "Connection closed"
    else do clientPrint $ "Received data : " ++ showByteString newData
            appendBuffer newData
            handler

-- | Responsible for parsing the "123:" part in the beginning of an SCGI request.
headerLengthReader :: ClientEnvironment
headerLengthReader = do
  buffer <- getBuffer
  case determineRequestLength buffer of
    RegularLengthResult maybeLength ->
      case maybeLength of
        Just (expectedLength, remainingBuffer) ->
          setBuffer remainingBuffer
          headerReader expectedLength
        Nothing ->
          readClientData headerLengthReader
    LengthStringConversionError -> do
      closeSocket

-- | Read the rest of the header of an SCGI request given its previously parsed size.
headerReader :: Int -> ClientEnvironment
headerReader headerSize = do
  buffer <- getBuffer
  if DB.length buffer >= headerSize
    then do let header = DB.take headerSize buffer
            dropBuffer headerSize
            case parseHeader header of
              Left errorMessage ->
                clientPrint errorMessage
                closeSocket
              Right requestHeader ->
                contentReader requestHeader
    else readClientData $ headerReader headerSize

-- | Read the actual content body after the SCGI header's terminating comma.
contentReader :: RequestHeader -> ClientEnvironment
contentReader header = do
  buffer <- getBuffer
  let contentLength = requestContentLength header
      bufferLength = DB.length buffer
  case compare bufferLength contentLength of
    LT ->
      readClientData contentReader
    EQ ->
      processRequest
    GT ->
      -- somethind odd occurred, an SCGI request was too long
      clientPrint "Request too long (" ++ show bufferLength " byte(s)) for the content length specified in the SCGI header (" ++ show contentLength ++ " byte(s))"
      closeSocket

-- | Process the request of a client.
processRequest :: ClientEnvironment
processRequest = do
  buffer <- getBuffer
  clientPrint $ "Processing request: " ++ showByteString buffer
  closeSocket