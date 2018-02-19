{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}



-- module

module Web.Sleep.Tumblr.Context (
       anonymously,
       withKey,
       withAuth,
       withBlog,
       with,
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
import           Network.HTTP.Client
import           Network.URI
import qualified Web.Authenticate.OAuth    as OA

import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Auth     (AuthCred)
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Methods
import           Web.Sleep.Tumblr.Query
import           Web.Sleep.Tumblr.Response



-- network interfaces

class Monad m => HasNetwork c m where
  send :: c -> Request -> m ByteString



-- helper functions for simple usage

anonymously :: MonadIO m => ReaderT NoContext m r -> m r
anonymously = undefined

withKey :: MonadIO m => APIKey -> ReaderT JustAPIKey m r -> m r
withKey key = with $ undefined key

withAuth :: MonadIO m => AuthCred -> ReaderT JustAuthCred m r -> m r
withAuth auth = with $ undefined auth

withBlog :: BlogId -> ReaderT (BlogContext c) m r -> ReaderT c m r
withBlog bid = withReaderT $ undefined . (bid,)

with :: c -> ReaderT c m r -> m r
with = flip runReaderT



-- actual network call and parsing

call  :: (QueryInfo q, HasNetwork c m, MonadReader c m, FromJSON' (QueryResult q))
          => Query q -> m (Either Error (QueryResult q))
callT :: (QueryInfo q, HasNetwork c m, MonadReader c m, FromJSON' (QueryResult q), MonadThrow m)
          => Query q -> m (QueryResult q)
callE :: (QueryInfo q, HasNetwork c m, MonadReader c m, FromJSON' (QueryResult q), MonadError Error m)
          => Query q -> m (QueryResult q)
call  q = ask >>= flip send (toRequest q) >>= decode
callT q = ask >>= flip send (toRequest q) >>= decodeT
callE q = ask >>= flip send (toRequest q) >>= decodeE



-- internal simple contexts

newtype TupleContext c1 c2 = TupleContext { getCtx :: (c1, c2) }
type    NoContext     = TupleContext Manager ()
type    JustAPIKey    = TupleContext Manager APIKey
type    JustAuthCred  = TupleContext Manager AuthCred
type    BlogContext   = TupleContext BlogId


instance HasAPIKey c2 => HasAPIKey (TupleContext c1 c2) where
  getAPIKey = getAPIKey . snd . getCtx

instance HasAuthCred c2 => HasAuthCred (TupleContext c1 c2) where
  getAuthCred = getAuthCred . snd . getCtx

instance HasHttpManager (TupleContext Manager c2) where
  getHttpManager = getHttpManager . fst . getCtx

instance HasHttpManager c2 => HasHttpManager (BlogContext c2) where
  getHttpManager = getHttpManager . snd . getCtx


instance MonadIO m => HasNetwork NoContext m where
  send _ = simpleHttp

instance MonadIO m => HasNetwork JustAPIKey m where
  send _ = simpleHttp

instance MonadIO m => HasNetwork JustAuthCred m where
  send _ = simpleHttp

instance (MonadIO m, HasNetwork c m) => HasNetwork (BlogContext c) m where
  send _ = undefined -- httpGet . snd . getCtx

simpleHttp = undefined



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
