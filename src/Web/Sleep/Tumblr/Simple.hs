{- |
Module: Web.Sleep.Tumblr.Simple

A collection of helpers aiming at providing a good enough out of the box
experience with the Tumblr API.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}



-- module


module Web.Sleep.Tumblr.Simple (

  -- * Monad aliases
  SimpleAPIKeyMonad,
  SimpleAPIKeyBlogMonad,
  SimpleAuthCredMonad,
  SimpleAuthCredBlogMonad,

  -- * Monadic helpers
  withAPIKey,
  withAuth,
  withBlog,

  -- * Auth helpers
  URLCallback,
  getSimpleAuthCred,
  getSimpleAuthCredM,
  getSimpleDebugAuthCred,

  ) where



-- imports

import           Control.Monad.Reader
import           Data.ByteString.Char8         (pack)
import qualified Network.HTTP.Client           as N
import qualified Web.Authenticate.OAuth        as OA

import           Web.Sleep.Common.Config
import           Web.Sleep.Common.Helpers.Base
import           Web.Sleep.Tumblr



-- type aliases for simple monad use

type SimpleAPIKeyMonad       m = ReaderT              (JustAPIKey   (Base m))  m
type SimpleAPIKeyBlogMonad   m = ReaderT (BlogContext (JustAPIKey   (Base m))) m
type SimpleAuthCredMonad     m = ReaderT              (JustAuthCred (Base m))  m
type SimpleAuthCredBlogMonad m = ReaderT (BlogContext (JustAuthCred (Base m))) m



-- helper functions for simple usage

{- | Takes your Tumblr API Key, and evaluates an monadic expression in a 'ReaderT'
   whose context is the key. It relies on 'MonadIO' to create a default
   'N.Manager'. Due to not having an authentication token, this can only be used
   to run unauthentified queries.

   > withApiKey key $ callT =<< getBlogInfo "myblog.tumblr.com"
-}
withAPIKey :: Config (Base m) -> APIKey -> SimpleAPIKeyMonad m r -> m r
withAPIKey conf key e = runReaderT e $ JustAPIKey conf key

{- | Similar to 'withAPIKey', except that it carries an authentication token
   instead of only having the key. All queries can therefore be used.

   > withAuth credentials $ callT =<< postNewBlogText "myblog.tumblr.com" "test"
-}
withAuth :: Config (Base m) -> AuthCred -> SimpleAuthCredMonad m r -> m r
withAuth conf auth e = runReaderT e $ JustAuthCred conf auth

{- | A wrapper around 'withReaderT' that adds a blog id to the current context,
   allowing you to use the query variants that do not explictly need the blog
   name.

   > withAPIKey k $ withBlog "myblog.tumblr.com" $ callT =<< getInfo
-}
withBlog :: BlogId -> ReaderT (BlogContext c) m r -> ReaderT c m r
withBlog bid = withReaderT $ BlogContext bid



-- auth simple helpers

type URLCallback m = String -> m String


{- | A very simple wrapper around "Web.Authenticate.OAuth"'s functions. The
   'URLCallback' paranameter is a callback that expects the authentication URL
   as an argument and returns the validation token as a result. Takes the
   original Tumblr 'OAuth' as an argument, returns the credentials after
   authenticating the user.
-}
getSimpleAuthCred :: MonadIO m => URLCallback m -> N.Manager -> OAuth -> m AuthCred
getSimpleAuthCred callback manager oauth = do
  tempCred <- OA.getTemporaryCredential oauth manager
  verifier <- callback $ OA.authorizeUrl oauth tempCred
  let newCred = OA.injectVerifier (pack verifier) tempCred
  cred     <- OA.getAccessToken oauth newCred manager
  return (oauth, cred)

{- | Same as 'getSimpleAuthCred', but expects to run in a 'MonadReader' that has a
   'N.Manager'.
-}
getSimpleAuthCredM :: (MonadIO m, MonadReader c m, N.HasHttpManager c) => URLCallback m -> OAuth -> m AuthCred
getSimpleAuthCredM callback oauth = do
  manager <- asks N.getHttpManager
  getSimpleAuthCred callback manager oauth

{- | Same as 'getSimpleAuthCred', but creates a default 'N.Manager'.
-}
getSimpleDebugAuthCred :: MonadIO m => URLCallback m -> OAuth -> m AuthCred
getSimpleDebugAuthCred callback oauth = do
  manager <- defaultManager
  getSimpleAuthCred callback manager oauth



-- internal simple contexts

data JustAPIKey   m = JustAPIKey   { jakConfig :: Config m, jakAPIKey   :: APIKey   }
data JustAuthCred m = JustAuthCred { jacConfig :: Config m, jacAuthCred :: AuthCred }
data BlogContext  c = BlogContext  { ctxBlog :: BlogId, ctx :: c }


instance HasAPIKey (JustAPIKey m) where
  getAPIKey = jakAPIKey

instance HasAPIKey (JustAuthCred m) where
  getAPIKey = APIKey . OA.oauthConsumerKey . fst . jacAuthCred

instance HasAPIKey c => HasAPIKey (BlogContext c) where
  getAPIKey = getAPIKey . ctx

instance MayHaveAuthCred (JustAPIKey   m) where maybeGetAuthCred = noAuthCred
instance MayHaveAuthCred (JustAuthCred m) where maybeGetAuthCred = justAuthCred

instance HasAuthCred (JustAuthCred m) where
  getAuthCred = jacAuthCred

instance MayHaveAuthCred c => MayHaveAuthCred (BlogContext c) where
  maybeGetAuthCred = maybeGetAuthCred . ctx

instance HasAuthCred c => HasAuthCred (BlogContext c) where
  getAuthCred = getAuthCred . ctx


instance HasBlogId (BlogContext c) where
  getBlogId = ctxBlog


instance HasConfig (JustAPIKey m) where
  type ConfigBase (JustAPIKey m) = m
  getConfig = jakConfig

instance HasConfig (JustAuthCred m) where
  type ConfigBase (JustAuthCred m) = m
  getConfig = jacConfig

instance HasConfig c => HasConfig (BlogContext c) where
  type ConfigBase (BlogContext c) = ConfigBase c
  getConfig = getConfig . ctx


instance N.HasHttpManager (JustAPIKey m) where
  getHttpManager = N.getHttpManager . jakConfig

instance N.HasHttpManager (JustAuthCred m) where
  getHttpManager = N.getHttpManager . jacConfig

instance N.HasHttpManager c => N.HasHttpManager (BlogContext c) where
  getHttpManager = N.getHttpManager . ctx
