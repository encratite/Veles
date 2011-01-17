module Veles.Server(
  runServer
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
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
runServer :: Int -> LockedConsole IO ()
runServer port = lift . runLockedConsole . lift . withSocketsDo $ do
  serverSocket <- socket AF_INET Stream defaultProtocol
  serverAddress <- inet_addr "127.0.0.1"
  bindSocket serverSocket $ SockAddrInet (fromIntegral port) serverAddress
  listen serverSocket 1
  putStrLn $ "Listening on port " ++ (show port)
  sequence_ . repeat $ acceptClient serverSocket

-- accept a new client from a server socket and create a new thread to process it
acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
  putStrLn "Waiting for a new connection"
  (clientSocket, clientAddress) <- accept serverSocket
  putStrLn $ "New connection: " ++ (show clientAddress)
  void . forkIO . processClient $ ConnectionInformation clientSocket clientAddress
  return ()

-- handles the communication with a client in its own thread created by acceptClient
processClient :: ConnectionInformation -> IO ()
processClient client = do
  clientData <- recv (connectionSocket client) 0x1000
  if DB.null clientData
    then putStrLn $ "Connection closed: " ++ clientAddress
    else do putStrLn $ "Received " ++ (show $ DB.length clientData) ++ " byte(s) from " ++ clientAddress ++ ":  "  ++ (show $ unpack clientData)
            processClient client
  where
    clientAddress = show $ connectionAddress client