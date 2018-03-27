{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE GADTs                 #-}
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
  liftSignOAuth,
  tumblrOAuth,
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
import           Data.ByteString
import qualified Network.HTTP.Client          as N
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
  default signOAuth :: (MonadSign b, MonadTrans n, n b ~ m) => AuthCred -> N.Request -> m N.Request
  signOAuth = liftSignOAuth

instance MonadSign IO where
  signOAuth = uncurry OA.signOAuth

instance MonadSign Identity where
  signOAuth (_, OA.Credential cred) = return . appendParams cred

instance (Monoid w, MonadSign m) => MonadSign (RWST r w s m)
instance (Monoid w, MonadSign m) => MonadSign (WriterT w m)
instance MonadSign m             => MonadSign (ContT r m)
instance MonadSign m             => MonadSign (ExceptT e m)
instance MonadSign m             => MonadSign (ListT m)
instance MonadSign m             => MonadSign (MaybeT m)
instance MonadSign m             => MonadSign (ReaderT r m)
instance MonadSign m             => MonadSign (StateT s m)
instance MonadSign m             => MonadSign (ResourceT m)



-- exported functions

liftSignOAuth :: (MonadSign b, MonadTrans m) => AuthCred -> N.Request -> m b N.Request
liftSignOAuth = lift ... signOAuth

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
