module Veles.Client(
  withClientEnvironment,
  getSocket,
  getBuffer,
  appendBuffer,
  dropBuffer,
  clientPrint
  ) where

import Control.Monad.State hiding (state)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC
import Data.Functor
import Network.Socket hiding (recv)

import Knyaz.Console

import Veles.ConnectionInformation

data ClientEnvironmentData = ClientEnvironmentData {
  clientConnection :: ConnectionInformation,
  clientBuffer :: DB.ByteString
  }

type ClientEnvironmentT m = StateT ClientEnvironmentData (LockedConsoleT m)

withClientEnvironment :: (MonadIO m, Functor m) => ConnectionInformation -> ClientEnvironmentT m a -> LockedConsoleT m ()
withClientEnvironment client state =
  void $ runStateT state $ ClientEnvironmentData client DBC.empty

getSocket :: (Monad m, Functor m) => ClientEnvironmentT m Socket
getSocket = connectionSocket <$> gets clientConnection

getBuffer :: Monad m => ClientEnvironmentT m DB.ByteString
getBuffer = gets clientBuffer

modifyBuffer :: Monad m => (DB.ByteString -> DB.ByteString) -> ClientEnvironmentT m ()
modifyBuffer f = modify modifier
  where
    modifier state = ClientEnvironmentData (clientConnection state) (f (clientBuffer state))

appendBuffer :: Monad m => DB.ByteString -> ClientEnvironmentT m ()
appendBuffer newData = modifyBuffer $ DBC.append newData

dropBuffer :: Monad m => Int -> ClientEnvironmentT m ()
dropBuffer n = modifyBuffer $ DBC.drop n

clientPrint :: MonadIO m => String -> ClientEnvironmentT m ()
clientPrint string = do
  connectionInformation <- gets clientConnection
  lift . printLine $ "[" ++ (show $ connectionAddress connectionInformation) ++ "] " ++ string