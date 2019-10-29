{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}



-- module

module Web.Sleep.Common.Network (
  NetworkConfig(..),
  MonadNetwork,
  QMethod(..),
  Operation(..),
  getNetworkConfig,
  defaultManager,
  defaultIONetworkConfig,
  makeMockNetworkConfig,
  ) where



-- imports

import           Control.Monad.Base
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy    as LB
import           Data.Has
import qualified Network.HTTP.Client     as N
import qualified Network.HTTP.Client.TLS as N
import qualified Web.Authenticate.OAuth  as OA

import           Web.Sleep.Libs.Request



-- request helpers

data QMethod = QGet | QPost deriving (Show, Eq)



-- network layer

data NetworkConfig m = NetworkConfig
  { networkManager :: N.Manager
  , requestSign    :: OA.OAuth -> OA.Credential -> N.Request -> m N.Request
  , networkSend    :: N.Request -> m (N.Response LB.ByteString)
  }

instance N.HasHttpManager (NetworkConfig m) where
  getHttpManager = networkManager

type MonadNetwork r b m = (MonadReader r m, Has (NetworkConfig b) r, MonadBase b m)



-- operation

class Operation from to err | from -> to err where
  toRequest :: from -> NetworkConfig m -> m N.Request
  fromResponse :: from -> N.Response LB.ByteString -> Either err to



-- convenience function

getNetworkConfig :: MonadNetwork r b m => m (NetworkConfig b)
getNetworkConfig = asks getter



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
