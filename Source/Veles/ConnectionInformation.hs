module Veles.ConnectionInformation(
  ConnectionInformation(..)
  ) where

import Network.Socket

data ConnectionInformation = ConnectionInformation {
  connectionSocket :: Socket,
  connectionAddress :: SockAddr
  }