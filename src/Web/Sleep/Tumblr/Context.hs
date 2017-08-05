{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}



-- module

module Web.Sleep.Tumblr.Context (
       HasHTTPGet(..),
       HasHTTPPost(..),
       anonymously,
       withKey,
       withAuth,
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
import           Network.HTTP.Conduit
import           Network.URL

import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Methods
import           Web.Sleep.Tumblr.Query
import           Web.Sleep.Tumblr.Response



-- network interfaces

class Monad m => HasHTTPGet c m where
  httpGet  :: c -> URL -> m ByteString

class Monad m => HasHTTPPost c m where
  httpPost :: c -> URL -> m ByteString



-- helper functions for simple usage

anonymously :: MonadIO m => ReaderT NoContext m r -> m r
anonymously = flip runReaderT NoContext

withKey :: MonadIO m => APIKey -> ReaderT JustAPIKey m r -> m r
withKey = flip runReaderT . JustAPIKey

withAuth :: APIKey -> AuthToken -> ReaderT AuthAndKey m r -> m r
withAuth k t = flip runReaderT $ AuthAndKey (k, t)



-- actual network call and parsing

call  :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c m, MonadReader c m, FromJSON' r)
          => Query q -> m (Either Error r)
callT :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c m, MonadReader c m, FromJSON' r, MonadThrow m)
          => Query q -> m r
callE :: (QueryResult q ~ r, HTTPMethod (QueryProtocol q) c m, MonadReader c m, FromJSON' r, MonadError Error m)
          => Query q -> m r
call  q = ask >>= getRawData q >>= decode
callT q = ask >>= getRawData q >>= decodeT
callE q = ask >>= getRawData q >>= decodeE



-- internal simple contexts

data    NoContext  = NoContext
newtype JustAPIKey = JustAPIKey APIKey
newtype AuthAndKey = AuthAndKey (APIKey, AuthToken)

instance HasAPIKey    JustAPIKey where getAPIKey    (JustAPIKey k)      = k
instance HasAPIKey    AuthAndKey where getAPIKey    (AuthAndKey (k, _)) = k
instance HasAuthToken AuthAndKey where getAuthToken (AuthAndKey (_, t)) = t

instance MonadIO m => HasHTTPGet NoContext m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPGet JustAPIKey m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPGet AuthAndKey m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPPost AuthAndKey m where
  httpPost _ = undefined



-- internal network method abstraction

class HTTPMethod (p :: QProtocol) c m where
  getRawData :: (QueryProtocol q ~ p, Monad m) => Query q -> c -> m ByteString

instance (HasHTTPGet c m) => HTTPMethod QGet c m where
  getRawData q c = httpGet c $ toURL q

instance (HasHTTPPost c m) => HTTPMethod QPost c m where
  getRawData q c = httpPost c $ toURL q



-- overlapping getResponse for ()

class FromJSON a => FromJSON' a where
  decode  :: Monad m            => ByteString -> m (Either Error a)
  decodeT :: MonadThrow m       => ByteString -> m a
  decodeE :: MonadError Error m => ByteString -> m a

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON' a where
  decode  = return . getResponse
  decodeT = getResponseT
  decodeE = getResponseE

instance {-# OVERLAPPING #-} FromJSON' () where
  decode  _ = return $ Right ()
  decodeT _ = return ()
  decodeE _ = return ()
