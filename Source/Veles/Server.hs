module Veles.Server(
  ConnectionInformation(..),
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
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
  -- forkReader . withClientEnvironment $ readClientData headerLengthReader
  consoleState <- ask
  let client = ClientEnvironmentData connectionInformation DBC.empty
      innerRunner = runStateT (readClientData headerLengthReader) client
      outerRunner = runReaderT innerRunner consoleState
  lift . void . forkIO . void $ outerRunner

showByteString :: DB.ByteString -> String
showByteString string = show $ DBC.unpack string

-- | This function specifies the size to be used for each recv call.
receiveSize :: Int
receiveSize = 0x1000

type ClientFlow = ClientEnvironmentT IO ()

readClientData :: ClientFlow -> ClientFlow
readClientData handler = do
  socket <- getSocket
  newData <- liftIO $ recv socket receiveSize
  if DB.null newData
    then clientPrint "Connection closed"
    else do clientPrint $ "Received data : " ++ showByteString newData
            appendBuffer newData
            handler

headerLengthReader :: ClientFlow
headerLengthReader = do
  buffer <- getBuffer
  case determineRequestLength buffer of
    RegularLengthResult maybeLength ->
      case maybeLength of
        Just (expectedLength, remainingBuffer) ->
          headerReader
        Nothing ->
          readClientData headerLengthReader
    LengthStringConversionError -> do
      socket <- getSocket
      lift . lift $ sClose socket


headerReader :: ClientFlow
headerReader = undefined

-- not implemented yet
processRequest :: ClientFlow
processRequest = do
  buffer <- getBuffer
  clientPrint $ "Processing request: " ++ showByteString buffer
  socket <- getSocket
  liftIO $ sClose socket