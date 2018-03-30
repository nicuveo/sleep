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
  appendHeader,
  setBody,
  HasNetwork(..),
  MonadNetwork,
  defaultManager,
  defaultSend,
  ) where



-- imports

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.List
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import qualified Data.ByteString              as EB
import qualified Data.ByteString.Lazy         as LB
import           Data.List                    (foldl')
import qualified Network.HTTP.Client          as N
import qualified Network.HTTP.Client.TLS      as N
import qualified Network.HTTP.Types.Header    as N

import           Web.Sleep.Common.Misc



-- request helpers

data QMethod = QGet | QPost deriving (Show, Eq)

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: a -> m N.Request



-- request helpers

appendParam :: (EB.ByteString, EB.ByteString) -> N.Request -> N.Request
appendParam p req = req { N.queryString = append p $ N.queryString req }
  where append (name, val) "" = EB.concat [         name, "=", val]
        append (name, val) qs = EB.concat [qs, "&", name, "=", val]

appendParams :: [(EB.ByteString, EB.ByteString)] -> N.Request -> N.Request
appendParams = flip $ foldl' $ flip appendParam

appendHeader :: N.HeaderName -> EB.ByteString -> N.Request -> N.Request
appendHeader hn hv req = req { N.requestHeaders = N.requestHeaders req ++ [ (hn, hv) ] }

setBody :: EB.ByteString -> N.Request -> N.Request
setBody body req = req { N.requestBody = N.RequestBodyBS body }



-- network monad

class Monad m => HasNetwork c m where
  send :: c -> N.Request -> m (N.Response LB.ByteString)
  default send :: (HasNetwork c b, MonadTrans n, n b ~ m) => c -> N.Request -> m (N.Response LB.ByteString)
  send = liftSend

instance (Monoid w, HasNetwork c m) => HasNetwork c (RWST r w s m)
instance (Monoid w, HasNetwork c m) => HasNetwork c (WriterT w m)
instance HasNetwork c m             => HasNetwork c (ContT r m)
instance HasNetwork c m             => HasNetwork c (ExceptT e m)
instance HasNetwork c m             => HasNetwork c (ListT m)
instance HasNetwork c m             => HasNetwork c (MaybeT m)
instance HasNetwork c m             => HasNetwork c (ReaderT c m)
instance HasNetwork c m             => HasNetwork c (StateT s m)
instance HasNetwork c m             => HasNetwork c (ResourceT m)

instance N.HasHttpManager c => HasNetwork c IO where
  send = defaultSend

type MonadNetwork c m = (MonadReader c m, HasNetwork c m)



-- simple network functions

liftSend :: (HasNetwork c b, MonadTrans m) => c -> N.Request -> m b (N.Response LB.ByteString)
liftSend = lift ... send

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultSend :: (MonadIO m, N.HasHttpManager c) => c -> N.Request -> m (N.Response LB.ByteString)
defaultSend c r = liftIO $ N.httpLbs r $ N.getHttpManager c
