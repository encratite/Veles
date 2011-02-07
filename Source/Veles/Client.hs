module Veles.Client(
  ClientEnvironmentData(..), -- temporary
  ClientEnvironmentT,
  withClientEnvironment,
  getSocket,
  closeSocket,
  getBuffer,
  setBuffer,
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

-- | Enters a new client environment which offers access to the connection information and the current buffer associated with the client.
withClientEnvironment :: (MonadIO m, Functor m) => ConnectionInformation -> ClientEnvironmentT m a -> LockedConsoleT m ()
withClientEnvironment client state =
  void $ runStateT state $ ClientEnvironmentData client DBC.empty

-- | Retrieve the socket of the client.
getSocket :: (Monad m, Functor m) => ClientEnvironmentT m Socket
getSocket = connectionSocket <$> gets clientConnection

-- | Close the connection of the client in the current client environment.
closeSocket :: (MonadIO m, Functor m) => ClientEnvironmentT m ()
closeSocket = liftIO . sClose =<< getSocket

-- | Retrieve the buffer of the client environment.
getBuffer :: Monad m => ClientEnvironmentT m DB.ByteString
getBuffer = gets clientBuffer

-- | Modifies the buffer of the client environment.
modifyBuffer :: Monad m => (DB.ByteString -> DB.ByteString) -> ClientEnvironmentT m ()
modifyBuffer f = modify modifier
  where
    modifier state = ClientEnvironmentData (clientConnection state) (f (clientBuffer state))

-- | Replaces the buffer of the client environment.
setBuffer :: Monad m => DB.ByteString -> ClientEnvironmentT m ()
setBuffer = modifyBuffer . const

-- | Append new data to the buffer of the client environment.
appendBuffer :: Monad m => DB.ByteString -> ClientEnvironmentT m ()
appendBuffer = modifyBuffer . DBC.append

-- | Drop data from the buffer of the client environment.
dropBuffer :: Monad m => Int -> ClientEnvironmentT m ()
dropBuffer = modifyBuffer . DBC.drop

-- | Print a string prefixed by an automatically generated string which identifies the client environment (i.e. address/port information).
clientPrint :: MonadIO m => String -> ClientEnvironmentT m ()
clientPrint string = do
  connectionInformation <- gets clientConnection
  lift . printLine $ "[" ++ (show $ connectionAddress connectionInformation) ++ "] " ++ string