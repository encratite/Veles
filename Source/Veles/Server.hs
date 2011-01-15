module Server( runServer
             ) where

import Network
import IO
import Control.Concurrent

data ConnectionInformation = ConnectionInformation { connectionHandle :: Handle
                                                   , connectionHostName :: HostName
                                                   , connectionPort :: PortNumber
                                                   }

runServer :: Int -> IO ()
runServer port = withSocketsDo $ do
  putStrLn $ "Listening on port " ++ (show port)
  serverSocket <- listenOn $ PortNumber $ fromIntegral port
  sequence_ $ repeat $ acceptClient serverSocket

acceptClient :: Socket -> IO ()
acceptClient serverSocket = do
  putStrLn "Waiting for a new connection"
  (handle, hostName, portNumber) <- accept serverSocket
  putStrLn $ "New connection: " ++ hostName ++ ":" ++ (show portNumber)
  forkIO $ clientThread $ ConnectionInformation handle hostName portNumber
  return ()

clientThread :: ConnectionInformation -> IO ()
clientThread client = do
  return ()