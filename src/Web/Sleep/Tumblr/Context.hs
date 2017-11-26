{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-}
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
import qualified Web.Authenticate.OAuth as OA

import           Web.Sleep.Tumblr.Auth (AuthCred)
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
anonymously = with NoContext

withKey :: MonadIO m => APIKey -> ReaderT JustAPIKey m r -> m r
withKey key = with $ JustAPIKey key

withAuth :: AuthCred -> ReaderT JustAuthCred m r -> m r
withAuth auth = with $ JustAuthCred auth

withBlog :: BlogId -> ReaderT (BlogContext c) m r -> ReaderT c m r
withBlog bid = withReaderT $ BlogContext . (bid,)

with :: c -> ReaderT c m r -> m r
with = flip runReaderT



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

data    NoContext     = NoContext
newtype JustAPIKey    = JustAPIKey   { justKey      :: APIKey      }
newtype JustAuthCred  = JustAuthCred { justAuthCred :: AuthCred    }
newtype BlogContext c = BlogContext  { blogContext  :: (BlogId, c) }


instance HasBlogId (BlogContext c) where
  getBlogId = fst . blogContext

instance HasAPIKey JustAPIKey where
  getAPIKey = justKey

instance HasAPIKey JustAuthCred where
  getAPIKey = APIKey . OA.oauthConsumerKey . fst . justAuthCred

instance HasAuthCred JustAuthCred where
  getAuthCred = justAuthCred

instance HasAPIKey c => HasAPIKey (BlogContext c) where
  getAPIKey = getAPIKey . snd . blogContext

instance HasAuthCred c => HasAuthCred (BlogContext c) where
  getAuthCred = getAuthCred . snd . blogContext


instance MonadIO m => HasHTTPGet NoContext m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPGet JustAPIKey m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPGet JustAuthCred m where
  httpGet _ = simpleHttp . exportURL

instance MonadIO m => HasHTTPPost JustAuthCred m where
  httpPost _ = undefined

instance (MonadIO m, HasHTTPGet c m) => HasHTTPGet (BlogContext c) m where
  httpGet = httpGet . snd . blogContext

instance (MonadIO m, HasHTTPPost c m) => HasHTTPPost (BlogContext c) m where
  httpPost = httpPost . snd . blogContext



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
