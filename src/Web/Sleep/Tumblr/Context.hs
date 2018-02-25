{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import qualified Network.HTTP.Client       as N
import qualified Web.Authenticate.OAuth    as OA

import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Auth     (AuthCred)
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Query
import           Web.Sleep.Tumblr.Response



-- network interfaces

class Monad m => HasNetwork c m where
  send :: c -> N.Request -> m ByteString



-- helper functions for simple usage

anonymously :: MonadIO m => ReaderT NoContext m r -> m r
anonymously e = do
  m <- defaultManager
  runReaderT e $ NoContext m

withKey :: MonadIO m => APIKey -> ReaderT JustAPIKey m r -> m r
withKey key e = do
  m <- defaultManager
  runReaderT e $ JustAPIKey m key

withAuth :: MonadIO m => AuthCred -> ReaderT JustAuthCred m r -> m r
withAuth auth e = do
  m <- defaultManager
  runReaderT e $ JustAuthCred m auth

withBlog :: BlogId -> ReaderT (BlogContext c) m r -> ReaderT c m r
withBlog bid = withReaderT $ BlogContext bid

with :: c -> ReaderT c m r -> m r
with = flip runReaderT



-- actual network call and parsing

call  :: (ToRequest q, HasNetwork c m, MonadReader c m, FromJSON' (RequestResult q))
          => q -> m (Either Error (RequestResult q))
callT :: (ToRequest q, HasNetwork c m, MonadReader c m, FromJSON' (RequestResult q), MonadThrow m)
          => q -> m (RequestResult q)
callE :: (ToRequest q, HasNetwork c m, MonadReader c m, FromJSON' (RequestResult q), MonadError Error m)
          => q -> m (RequestResult q)
call  q = ask >>= flip send (toRequest q) >>= decode
callT q = ask >>= flip send (toRequest q) >>= decodeT
callE q = ask >>= flip send (toRequest q) >>= decodeE



-- internal simple contexts

newtype NoContext     = NoContext    { ctxManager :: N.Manager }
data    JustAPIKey    = JustAPIKey   { ctxManager :: N.Manager, ctxAPIKey :: APIKey }
data    JustAuthCred  = JustAuthCred { ctxManager :: N.Manager, ctxAuthCred :: AuthCred }
data    BlogContext c = BlogContext  { ctxBlog :: BlogId, ctx :: c }


instance HasAPIKey JustAPIKey where
  getAPIKey = ctxAPIKey

instance HasAPIKey JustAuthCred where
  getAPIKey = APIKey . OA.oauthConsumerKey . fst . ctxAuthCred

instance HasAPIKey c => HasAPIKey (BlogContext c) where
  getAPIKey = getAPIKey . ctx


instance HasAuthCred JustAuthCred where
  addAuth = undefined -- uncurry OA.signOAuth . ctxAuthCred

instance HasAuthCred c => HasAuthCred (BlogContext c) where
  addAuth = addAuth . ctx


instance HasBlogId (BlogContext c) where
  getBlogId = ctxBlog


instance N.HasHttpManager NoContext where
  getHttpManager = ctxManager

instance N.HasHttpManager JustAPIKey where
  getHttpManager = ctxManager

instance N.HasHttpManager JustAuthCred where
  getHttpManager = ctxManager

instance N.HasHttpManager c => N.HasHttpManager (BlogContext c) where
  getHttpManager = N.getHttpManager . ctx


instance MonadIO m => HasNetwork NoContext m where
  send c r = liftIO $ fmap N.responseBody $ N.httpLbs r $ N.getHttpManager c

instance MonadIO m => HasNetwork JustAPIKey m where
  send c r = liftIO $ fmap N.responseBody $ N.httpLbs r $ N.getHttpManager c

instance MonadIO m => HasNetwork JustAuthCred m where
  send c r = liftIO $ fmap N.responseBody $ N.httpLbs r $ N.getHttpManager c

instance HasNetwork c m => HasNetwork (BlogContext c) m where
  send = send . ctx



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



-- local helpers

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.defaultManagerSettings
