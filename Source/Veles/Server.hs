module Server(
  runServer
  ) where

import Control.Concurrent
import qualified Data.ByteString as DB
import Data.ByteString.Char8 (unpack)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

data ConnectionInformation = ConnectionInformation {
  connectionSocket :: Socket,
  connectionAddress :: SockAddr
  }

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
  serverSocket <- socket AF_INET Stream defaultProtocol
  serverAddress <- inet_addr "127.0.0.1"
  bindSocket serverSocket $ SockAddrInet (fromIntegral port) serverAddress
  listen serverSocket 1
  putStrLn $ "Listening on port " ++ (show port)
  sequence_ $ repeat $ acceptClient serverSocket

acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
  putStrLn "Waiting for a new connection"
  (clientSocket, clientAddress) <- accept serverSocket
  putStrLn $ "New connection: " ++ (show clientAddress)
  forkIO $ processClient $ ConnectionInformation clientSocket clientAddress
  return ()

processClient :: ConnectionInformation -> IO ()
processClient client = do
  clientData <- recv (connectionSocket client) 0x1000
  if DB.null clientData
    then (putStrLn $ "Connection closed: " ++ clientAddress) >>
         return ()
    else (putStrLn $ "Received " ++ (show (DB.length clientData)) ++ " byte(s) from " ++ clientAddress ++ ": " ++ (unpack clientData)) >>
         processClient client
  where
    clientAddress = (show (connectionAddress client))