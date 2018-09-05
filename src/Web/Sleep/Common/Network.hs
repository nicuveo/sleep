{-# LANGUAGE ConstraintKinds #-}



-- module

module Web.Sleep.Common.Network (
  NetworkConfig(..),
  HasNetworkConfig(..),
  MonadNetwork,
  QMethod(..),
  ToRequest(..),
  defaultManager,
  defaultIONetworkConfig,
  makeMockNetworkConfig,
  ) where



-- imports

import           Control.Monad.Cont
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy             as LB
import qualified Network.HTTP.Client              as N
import qualified Network.HTTP.Client.TLS          as N
import qualified Web.Authenticate.OAuth           as OA

import           Web.Sleep.Libs.Base
import           Web.Sleep.Libs.Request



-- request helpers

data QMethod = QGet | QPost deriving (Show, Eq)

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: MonadNetwork r m => a -> m N.Request



-- network layer

data NetworkConfig m = NetworkConfig
  { networkManager :: N.Manager
  , requestSign    :: OA.OAuth -> OA.Credential -> N.Request -> m N.Request
  , networkSend    :: N.Request -> m (N.Response LB.ByteString)
  }

class HasNetworkConfig a where
  type NetworkConfigBase a :: * -> *
  getNetworkConfig :: a -> NetworkConfig (NetworkConfigBase a)

instance HasNetworkConfig (NetworkConfig m) where
  type NetworkConfigBase (NetworkConfig m) = m
  getNetworkConfig = id

instance N.HasHttpManager (NetworkConfig m) where
  getHttpManager = networkManager

type MonadNetwork r m = (MonadReader r m, MonadBase m, HasNetworkConfig r, Base m ~ NetworkConfigBase r)



-- simple network usage

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultIONetworkConfig :: N.Manager -> NetworkConfig IO
defaultIONetworkConfig m = NetworkConfig m sign send
  where sign = OA.signOAuth
        send = flip N.httpLbs m

makeMockNetworkConfig :: [(String, String)] -> NetworkConfig Identity
makeMockNetworkConfig reqMap = NetworkConfig manager sign send
  where sign _ (OA.Credential creds) = return . appendParams creds
        send req = return $ maybe undefined undefined $ lookup (show req) reqMap
        manager = error "tried to access mock network manager"
