{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Tumblr.Context (
       HasHTTPGet(..),
       HasHTTPPost(..),
       call,
       callT,
       callE,
       ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson.Types
import           Data.ByteString.Lazy
import           Data.Proxy
import           Network.URL

import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Query
import           Web.Sleep.Tumblr.Response



-- exported types

class HasHTTPGet c where
  httpGet  :: Monad m => c -> URL -> m ByteString

class HasHTTPPost c where
  httpPost :: Monad m => c -> URL -> m ByteString



-- exported functions

call  :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c, MonadReader c m, FromJSON r)
          => Query q -> m (Either Error r)
callT :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c, MonadReader c m, FromJSON r, MonadThrow m)
          => Query q -> m r
callE :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c, MonadReader c m, FromJSON r, MonadError Error m)
          => Query q -> m r
call  q = fmap getResponse $ getRawData q =<< ask
callT q = getResponseT   =<< getRawData q =<< ask
callE q = getResponseE   =<< getRawData q =<< ask



-- internal helpers

class HTTPMethod (p :: QProtocol) c where
  getRawData :: (QueryProtocol q ~ p, Monad m) => Query q -> c -> m ByteString

instance (HasHTTPGet c) => HTTPMethod QGet c where
  getRawData q c = httpGet c $ toURL q

instance (HasHTTPPost c) => HTTPMethod QPost c where
  getRawData q c = httpPost c $ toURL q
