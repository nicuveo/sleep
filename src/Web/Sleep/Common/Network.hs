{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-

Network related helper types and functions.

-}



-- module

module Web.Sleep.Common.Network (
  QMethod(..),
  ToRequest(..),
  appendParam,
  appendParams,
  HasNetwork(..),
  MonadNetwork,
  defaultManager,
  defaultSend,
  ) where



-- imports

import           Control.Monad.Cont
import           Control.Monad.Reader
import qualified Data.ByteString         as EB
import qualified Data.ByteString.Lazy    as LB
import           Data.List               (foldl')
import qualified Network.HTTP.Client     as N
import qualified Network.HTTP.Client.TLS as N



-- request helpers

data QMethod = QGet | QPost

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: a -> m N.Request



-- query string helpers

appendParam :: (EB.ByteString, EB.ByteString) -> N.Request -> N.Request
appendParam p req = req { N.queryString = append p $ N.queryString req }
  where append (name, val) "" = EB.concat [         name, "=", val]
        append (name, val) qs = EB.concat [qs, "&", name, "=", val]

appendParams :: [(EB.ByteString, EB.ByteString)] -> N.Request -> N.Request
appendParams = flip $ foldl' $ flip appendParam



-- network monad

class HasNetwork c m where
  send :: c -> N.Request -> m (N.Response LB.ByteString)
  default send :: (MonadIO m, N.HasHttpManager c) => c -> N.Request -> m (N.Response LB.ByteString)
  send = defaultSend

type MonadNetwork c m = (MonadReader c m, HasNetwork c m)



-- simple network functions

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultSend :: (MonadIO m, N.HasHttpManager c) => c -> N.Request -> m (N.Response LB.ByteString)
defaultSend c r = liftIO $ N.httpLbs r $ N.getHttpManager c
