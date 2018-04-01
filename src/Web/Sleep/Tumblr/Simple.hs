{- |
Module: Web.Sleep.Tumblr.Simple

A collection of helpers aiming at providing a good enough out of the box
experience with the Tumblr API.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module


module Web.Sleep.Tumblr.Simple (

  -- * Monad aliases
  SimpleAPIKeyMonad,
  SimpleAPIKeyBlogMonad,
  SimpleAuthCredMonad,
  SimpleAuthCredBlogMonad,

  -- * Monadic helpers
  withAPIKey,
  withManagerAndAPIKey,
  withAuth,
  withManagerAndAuth,
  withBlog,

  -- * Auth helpers
  URLCallback,
  getSimpleAuthCred,
  getSimpleAuthCredM,
  getSimpleDebugAuthCred,

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

{- | Takes your Tumblr API Key, and evaluates an monadic expression in a 'ReaderT'
   whose context is the key. It relies on 'MonadIO' to create a default
   'N.Manager'. Due to not having an authentication token, this can only be used
   to run unauthentified queries.

   > withApiKey key $ callT =<< getBlogInfo "myblog.tumblr.com"
-}
withAPIKey :: MonadIO m => APIKey -> SimpleAPIKeyMonad m r -> m r
withAPIKey key e = defaultManager >>= \m -> withManagerAndAPIKey m key e

{- | Does the same, but instead of creating a 'N.Manager' it uses the one given as
   an argument.

   > withManagerAndApiKey n key $ callT =<< getBlogInfo "myblog.tumblr.com"
-}
withManagerAndAPIKey :: N.Manager -> APIKey -> SimpleAPIKeyMonad m r -> m r
withManagerAndAPIKey m key e = runReaderT e $ JustAPIKey m key

{- | Similar to 'withAPIKey', except that it carries an authentication token
   instead of only having the key. All queries can therefore be used.

   > withAuth credentials $ callT =<< postNewBlogText "myblog.tumblr.com" "test"
-}
withAuth :: MonadIO m => AuthCred -> SimpleAuthCredMonad m r -> m r
withAuth auth e = defaultManager >>= \m -> withManagerAndAuth m auth e

{- | Does the same, but instead of creating an 'N.Manager' it uses the one given
   as an argument.

   > withManagerAndAuth m credentials $ callT =<< postNewBlogText "myblog.tumblr.com" "test"
-}
withManagerAndAuth :: N.Manager -> AuthCred -> SimpleAuthCredMonad m r -> m r
withManagerAndAuth m auth e = runReaderT e $ JustAuthCred m auth

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
