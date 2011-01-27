module Veles.Client(
  withClientEnvironment,
  getSocket,
  getBuffer,
  setBuffer,
  clientPrint
  ) where

import Control.Monad.State hiding (state)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import Network.Socket hiding (recv)

import Knyaz.Console

import Veles.ConnectionInformation

data ClientEnvironmentData = ClientEnvironmentData {
  clientConnection :: ConnectionInformation,
  clientBuffer :: DB.ByteString
  }

type ClientEnvironmentT m = StateT ClientEnvironmentData (LockedConsoleT m)

withClientEnvironment :: (MonadIO m, Functor m) => ConnectionInformation -> ClientEnvironmentT m a -> LockedConsoleT m ()
withClientEnvironment client state = do
  let environment = ClientEnvironmentData client DBC.empty
  runStateT state environment
  return ()

getSocket :: Monad m => ClientEnvironmentT m Socket
getSocket = do
  connection <- gets clientConnection
  return $ connectionSocket connection

getBuffer :: Monad m => ClientEnvironmentT m DB.ByteString
getBuffer = gets clientBuffer

setBuffer :: Monad m => DB.ByteString -> ClientEnvironmentT m ()
setBuffer newBuffer = do
  connection <- gets clientConnection
  put $ ClientEnvironmentData connection newBuffer

clientPrint :: MonadIO m => String -> ClientEnvironmentT m ()
clientPrint string = do
  connectionInformation <- gets clientConnection
  lift . printLine $ "[" ++ (show $ connectionAddress connectionInformation) ++ "] " ++ string