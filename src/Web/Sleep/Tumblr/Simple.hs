{- |
Module: Web.Sleep.Tumblr.Simple

A collection of helpers aiming at providing a good enough out of the box
experience with the Tumblr API.
-}




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
  checkAuthValidity

  ) where



-- imports

import           Control.Monad.Reader
import           Data.ByteString.Char8         (pack)
import qualified Network.HTTP.Client           as N
import qualified Web.Authenticate.OAuth        as OA

import           Web.Sleep.Libs.Base

import           Web.Sleep.Common.Network
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

   > withApiKey networkLayer key $ callT $ getBlogInfo "myblog.tumblr.com"
-}
withAPIKey :: NetworkConfig (Base m) -> APIKey -> SimpleAPIKeyMonad m r -> m r
withAPIKey nl key e = runReaderT e $ JustAPIKey nl key

{- | Similar to 'withAPIKey', except that it carries an authentication token
   instead of only having the key. All queries can therefore be used.

   > withAuth networkLayer credentials $ callT =<< postNewBlogText "myblog.tumblr.com" "test"
-}
withAuth :: NetworkConfig (Base m) -> AuthCred -> SimpleAuthCredMonad m r -> m r
withAuth nl auth e = runReaderT e $ JustAuthCred nl auth

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

checkAuthValidity :: MonadMaybeAuth r m => m Bool
checkAuthValidity = do
  res <- call $ getBlogInfo "test.tumblr.com"
  return $ either (== undefined) (const False) res



-- internal simple contexts

data JustAPIKey   m = JustAPIKey   { jakNetworkConfig :: NetworkConfig m, jakAPIKey   :: APIKey   }
data JustAuthCred m = JustAuthCred { jacNetworkConfig :: NetworkConfig m, jacAuthCred :: AuthCred }
data BlogContext  c = BlogContext  { ctxBlog :: BlogId, ctx :: c }


instance HasAPIKey (JustAPIKey m) where
  getAPIKey = jakAPIKey

instance HasAPIKey (JustAuthCred m) where
  getAPIKey = APIKey . OA.oauthConsumerKey . fst . jacAuthCred

instance HasAPIKey c => HasAPIKey (BlogContext c) where
  getAPIKey = getAPIKey . ctx


instance MayHaveAuthCred (JustAuthCred m)
instance HasAuthCred (JustAuthCred m) where
  getAuthCred = jacAuthCred

instance MayHaveAuthCred c => MayHaveAuthCred (BlogContext c) where
  maybeGetAuthCred = maybeGetAuthCred . ctx

instance HasAuthCred c => HasAuthCred (BlogContext c) where
  getAuthCred = getAuthCred . ctx


instance HasBlogId (BlogContext c) where
  getBlogId = ctxBlog


instance HasNetworkConfig (JustAPIKey m) where
  type NetworkConfigBase (JustAPIKey m) = m
  getNetworkConfig = jakNetworkConfig

instance HasNetworkConfig (JustAuthCred m) where
  type NetworkConfigBase (JustAuthCred m) = m
  getNetworkConfig = jacNetworkConfig

instance HasNetworkConfig c => HasNetworkConfig (BlogContext c) where
  type NetworkConfigBase (BlogContext c) = NetworkConfigBase c
  getNetworkConfig = getNetworkConfig . ctx


instance N.HasHttpManager (JustAPIKey m) where
  getHttpManager = N.getHttpManager . jakNetworkConfig

instance N.HasHttpManager (JustAuthCred m) where
  getHttpManager = N.getHttpManager . jacNetworkConfig

instance N.HasHttpManager c => N.HasHttpManager (BlogContext c) where
  getHttpManager = N.getHttpManager . ctx
