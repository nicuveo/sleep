{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Simple (
  SimpleAnonymousMonad,
  SimpleAPIKeyMonad,
  SimpleAPIKeyBlogMonad,
  SimpleAuthCredMonad,
  SimpleAuthCredBlogMonad,
  anonymously,
  withAPIKey,
  withAuth,
  withBlog,
  getSimpleAuthCred,
  getSimpleDebugAuthCred,
  getSimpleAuthCredM,
  ) where



-- imports

import           Control.Monad.Reader
import           Data.ByteString.Char8  (pack)
import qualified Network.HTTP.Client    as N
import qualified Web.Authenticate.OAuth as OA

import           Web.Sleep.Tumblr



-- type aliases for simple monad use

type SimpleAnonymousMonad    = ReaderT NoContext
type SimpleAPIKeyMonad       = ReaderT JustAPIKey
type SimpleAPIKeyBlogMonad   = ReaderT (BlogContext JustAPIKey)
type SimpleAuthCredMonad     = ReaderT JustAuthCred
type SimpleAuthCredBlogMonad = ReaderT (BlogContext JustAuthCred)



-- helper functions for simple usage

anonymously :: MonadIO m => SimpleAnonymousMonad m r -> m r
anonymously e = do
  m <- defaultManager
  runReaderT e $ NoContext m

withAPIKey :: MonadIO m => APIKey -> SimpleAPIKeyMonad m r -> m r
withAPIKey key e = do
  m <- defaultManager
  runReaderT e $ JustAPIKey m key

withAuth :: MonadIO m => AuthCred -> SimpleAuthCredMonad m r -> m r
withAuth auth e = do
  m <- defaultManager
  runReaderT e $ JustAuthCred m auth

withBlog :: BlogId -> ReaderT (BlogContext c) m r -> ReaderT c m r
withBlog bid = withReaderT $ BlogContext bid



-- auth simple helpers

type URLCallback m = String -> m String

getSimpleAuthCred :: MonadIO m => URLCallback m -> N.Manager -> OAuth -> m AuthCred
getSimpleAuthCred callback manager oauth = do
  tempCred <- OA.getTemporaryCredential oauth manager
  verifier <- callback $ OA.authorizeUrl oauth tempCred
  let newCred = OA.injectVerifier (pack verifier) tempCred
  cred     <- OA.getAccessToken oauth newCred manager
  return (oauth, cred)

getSimpleAuthCredM :: (MonadIO m, MonadReader c m, N.HasHttpManager c) => URLCallback m -> OAuth -> m AuthCred
getSimpleAuthCredM callback oauth = do
  manager <- asks N.getHttpManager
  getSimpleAuthCred callback manager oauth

getSimpleDebugAuthCred :: MonadIO m => URLCallback m -> OAuth -> m AuthCred
getSimpleDebugAuthCred callback oauth = do
  manager <- defaultManager
  getSimpleAuthCred callback manager oauth



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


instance MayHaveAuthCred JustAuthCred
instance HasAuthCred JustAuthCred where
  getAuthCred = ctxAuthCred

instance MayHaveAuthCred c => MayHaveAuthCred (BlogContext c) where
  maybeGetAuthCred = maybeGetAuthCred . ctx

instance HasAuthCred c => HasAuthCred (BlogContext c) where
  getAuthCred = getAuthCred . ctx


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


instance MonadIO m => HasNetwork NoContext       m
instance MonadIO m => HasNetwork JustAPIKey      m
instance MonadIO m => HasNetwork JustAuthCred    m

instance HasNetwork c m => HasNetwork (BlogContext c) m where
  send c = send $ ctx c
