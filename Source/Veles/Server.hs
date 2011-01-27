module Veles.Server(
  ConnectionInformation(..),
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
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
  forkReader . withClientEnvironment $ readClientData headerLengthReader

showByteString :: DB.ByteString -> String
showByteString string = show $ DBC.unpack string

-- | This function specifies the size to be used for each recv call.
receiveSize :: Int
receiveSize = 0x1000

type ClientFlow = ClientEnvironment IO ()

readClientData :: ClientFlow -> ClientFlow
readClientData handler = do
  let clientSocket = connectionSocket client
  clientData <- liftIO $ recv clientSocket receiveSize
  let newBuffer = DB.append buffer clientData
      currentLength = DB.length newBuffer
  if DB.null clientData
    then do putStrLn $ "Connection closed: " ++ clientAddress
    else do putStrLn $ "Received data from " ++ clientAddress ++ ": " ++ showByteString clientData
            handler newBuffer
  where
    clientAddress = show $ connectionAddress client

headerLengthReader :: ClientFlow
headerLengthReader =
  case determineRequestLength getBuffer of
    RegularLengthResult maybeLength ->
      case maybeLength of
        Just (expectedLength, remainingBuffer) ->
          headerReader
        Nothing ->
          readClientData headerLengthReader
    LengthStringConversionError ->
      liftIO $ sClose getSocket


headerReader :: ClientFlow
headerReader = undefined

-- not implemented yet
processRequest :: ConnectionInformation -> DB.ByteString -> ClientFlow
processRequest client request = do
  let clientSocket = connectionSocket client
  printLine $ "Processing request: " ++ showByteString request
  liftIO $ sClose clientSocket