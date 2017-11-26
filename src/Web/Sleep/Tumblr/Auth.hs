{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Auth (
  -- exported types
  AppName,
  AppKey,
  AppSecret,
  OAuth,
  Credential,
  AuthCred,
  -- exported functions
  tumblrOAuth,
  getAuthCred,
  getDebugAuthCred,
  getAuthCredM,
  ) where



-- imports

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString
import qualified Network.HTTP.Client      as N
import qualified Web.Authenticate.OAuth   as OA

import           Web.Sleep.Common.Network



-- type aliases

type AppName    = String
type AppKey     = ByteString
type AppSecret  = ByteString
type OAuth      = OA.OAuth
type Credential = OA.Credential
type AuthCred   = (OAuth, Credential)



-- exported functions

tumblrOAuth :: AppName -> AppKey -> AppSecret -> OAuth
tumblrOAuth name key secret =
  OA.newOAuth { OA.oauthServerName     = name
              , OA.oauthRequestUri     = "http://www.tumblr.com/oauth/request_token"
              , OA.oauthAuthorizeUri   = "http://www.tumblr.com/oauth/authorize"
              , OA.oauthAccessTokenUri = "http://www.tumblr.com/oauth/access_token"
              , OA.oauthConsumerKey    = key
              , OA.oauthConsumerSecret = secret
              }


-- helpers

type URLCallback m = String -> m ()

getAuthCred :: MonadIO m => URLCallback m -> N.Manager -> OAuth -> m AuthCred
getAuthCred callback manager oauth = do
  tempcred <- OA.getTemporaryCredential oauth manager
  callback $ OA.authorizeUrl oauth tempcred
  cred <- OA.getAccessToken oauth tempcred manager
  return (oauth, cred)

getAuthCredM :: MonadManager r m => URLCallback m -> OAuth -> m AuthCred
getAuthCredM callback oauth = do
  manager <- asks N.getHttpManager
  getAuthCred callback manager oauth

getDebugAuthCred :: MonadIO m => URLCallback m -> OAuth -> m AuthCred
getDebugAuthCred callback oauth = do
  manager <- liftIO $ N.newManager N.defaultManagerSettings
  getAuthCred callback manager oauth
