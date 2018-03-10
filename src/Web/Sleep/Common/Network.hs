{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-

Network related helper types and functions.

-}



-- module

module Web.Sleep.Common.Network (
  QMethod(..),
  ToRequest(..),
  HasNetwork(..),
  MonadNetwork,
  defaultManager,
  defaultSend,
  ) where



-- imports

import           Control.Monad.Cont
import           Control.Monad.Reader
import           Data.ByteString.Lazy
import qualified Network.HTTP.Client     as N
import qualified Network.HTTP.Client.TLS as N



-- request helpers

data QMethod = QGet | QPost

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: a -> m N.Request



-- network monad

class HasNetwork c m where
  send :: c -> N.Request -> m ByteString
  default send :: (MonadIO m, N.HasHttpManager c) => c -> N.Request -> m ByteString
  send = defaultSend

type MonadNetwork c m = (MonadReader c m, HasNetwork c m)



-- simple network functions

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultSend :: (MonadIO m, N.HasHttpManager c) => c -> N.Request -> m ByteString
defaultSend c r = liftIO $ fmap N.responseBody $ N.httpLbs r $ N.getHttpManager c
