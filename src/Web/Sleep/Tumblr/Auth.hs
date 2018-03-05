{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Auth (
  -- exported types
  AppKey,
  AppSecret,
  OAuth,
  Credential,
  AuthCred,
  -- exported classes
  MonadSign(..),
  -- exported functions
  tumblrOAuth,
  getSimpleAuthCred,
  getSimpleDebugAuthCred,
  getSimpleAuthCredM,
  ) where



-- imports

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.List
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.ByteString.Char8        (ByteString, pack)
import qualified Network.HTTP.Client          as N
import qualified Network.HTTP.Client.TLS      as N
import qualified Web.Authenticate.OAuth       as OA

import           Web.Sleep.Common.Misc
import           Web.Sleep.Common.Network



-- type aliases

type AppKey     = ByteString
type AppSecret  = ByteString
type OAuth      = OA.OAuth
type Credential = OA.Credential
type AuthCred   = (OAuth, Credential)



-- signing class

class Monad m => MonadSign m where
  signOAuth :: AuthCred -> N.Request -> m N.Request
  default signOAuth :: MonadIO m => AuthCred -> N.Request -> m N.Request
  signOAuth = liftIO ... uncurry OA.signOAuth

instance MonadSign IO
instance MonadSign Identity where
  signOAuth _ = return

instance (Monoid w, MonadSign m) => MonadSign (RWST r w s m) where signOAuth = lift ... signOAuth
instance (Monoid w, MonadSign m) => MonadSign (WriterT w m)  where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (ContT r m)    where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (ExceptT e m)  where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (ListT m)      where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (MaybeT m)     where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (ReaderT r m)  where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (StateT s m)   where signOAuth = lift ... signOAuth
instance MonadSign m             => MonadSign (ResourceT m)  where signOAuth = lift ... signOAuth



-- exported functions

tumblrOAuth :: AppKey -> AppSecret -> OAuth
tumblrOAuth key secret =
  OA.newOAuth { OA.oauthServerName      = "tumblr"
              , OA.oauthRequestUri      = "https://www.tumblr.com/oauth/request_token"
              , OA.oauthAuthorizeUri    = "https://www.tumblr.com/oauth/authorize"
              , OA.oauthAccessTokenUri  = "https://www.tumblr.com/oauth/access_token"
              , OA.oauthVersion         = OA.OAuth10a
              , OA.oauthSignatureMethod = OA.HMACSHA1
              , OA.oauthConsumerKey     = key
              , OA.oauthConsumerSecret  = secret
              }



-- helpers

type URLCallback m = String -> m String

getSimpleAuthCred :: MonadIO m => URLCallback m -> N.Manager -> OAuth -> m AuthCred
getSimpleAuthCred callback manager oauth = do
  tempCred <- OA.getTemporaryCredential oauth manager
  verifier <- callback $ OA.authorizeUrl oauth tempCred
  let newCred = OA.injectVerifier (pack verifier) tempCred
  cred     <- OA.getAccessToken oauth newCred manager
  return (oauth, cred)

getSimpleAuthCredM :: MonadManager r m => URLCallback m -> OAuth -> m AuthCred
getSimpleAuthCredM callback oauth = do
  manager <- asks N.getHttpManager
  getSimpleAuthCred callback manager oauth

getSimpleDebugAuthCred :: MonadIO m => URLCallback m -> OAuth -> m AuthCred
getSimpleDebugAuthCred callback oauth = do
  manager <- liftIO $ N.newManager N.tlsManagerSettings
  getSimpleAuthCred callback manager oauth
