module Veles.Server(
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as DB
import Data.ByteString.Char8 (unpack)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import Knyaz.Console

data ConnectionInformation = ConnectionInformation {
  connectionSocket :: Socket,
  connectionAddress :: SockAddr
  }

-- run the SCGI server on the specified port, does not terminate
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

-- accept a new client from a server socket and create a new thread to process it
acceptClient :: PrintFunction -> Socket -> IO ()
acceptClient printLine serverSocket = do
  printLine "Waiting for a new connection"
  (clientSocket, clientAddress) <- accept serverSocket
  printLine $ "New connection: " ++ (show clientAddress)
  void . forkIO . processClient printLine $ ConnectionInformation clientSocket clientAddress
  return ()

-- handles the communication with a client in its own thread created by acceptClient
processClient :: PrintFunction -> ConnectionInformation -> IO ()
processClient printLine client = do
  clientData <- recv (connectionSocket client) 0x1000
  if DB.null clientData
    then printLine $ "Connection closed: " ++ clientAddress
    else do printLine $ "Received " ++ (show $ DB.length clientData) ++ " byte(s) from " ++ clientAddress ++ ":  "  ++ (show $ unpack clientData)
            processClient printLine client
  where
    clientAddress = show $ connectionAddress client