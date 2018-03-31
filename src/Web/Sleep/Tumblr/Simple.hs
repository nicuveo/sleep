{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Simple (
  SimpleAPIKeyMonad,
  SimpleAPIKeyBlogMonad,
  SimpleAuthCredMonad,
  SimpleAuthCredBlogMonad,
  withAPIKey,
  withManagerAndAPIKey,
  withAuth,
  withManagerAndAuth,
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

type SimpleAPIKeyMonad       = ReaderT JustAPIKey
type SimpleAPIKeyBlogMonad   = ReaderT (BlogContext JustAPIKey)
type SimpleAuthCredMonad     = ReaderT JustAuthCred
type SimpleAuthCredBlogMonad = ReaderT (BlogContext JustAuthCred)



-- helper functions for simple usage

withAPIKey :: MonadIO m => APIKey -> SimpleAPIKeyMonad m r -> m r
withAPIKey key e = defaultManager >>= \m -> withManagerAndAPIKey m key e

withManagerAndAPIKey :: N.Manager -> APIKey -> SimpleAPIKeyMonad m r -> m r
withManagerAndAPIKey m key e = runReaderT e $ JustAPIKey m key

withAuth :: MonadIO m => AuthCred -> SimpleAuthCredMonad m r -> m r
withAuth auth e = defaultManager >>= \m -> withManagerAndAuth m auth e

withManagerAndAuth :: N.Manager -> AuthCred -> SimpleAuthCredMonad m r -> m r
withManagerAndAuth m auth e = runReaderT e $ JustAuthCred m auth

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

data JustAPIKey    = JustAPIKey   { ctxManager :: N.Manager, ctxAPIKey :: APIKey }
data JustAuthCred  = JustAuthCred { ctxManager :: N.Manager, ctxAuthCred :: AuthCred }
data BlogContext c = BlogContext  { ctxBlog :: BlogId, ctx :: c }


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


instance N.HasHttpManager JustAPIKey where
  getHttpManager = ctxManager

instance N.HasHttpManager JustAuthCred where
  getHttpManager = ctxManager

instance N.HasHttpManager c => N.HasHttpManager (BlogContext c) where
  getHttpManager = N.getHttpManager . ctx
