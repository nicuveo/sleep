{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Auth where



-- imports

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString
import qualified Network.HTTP.Client    as N
import qualified Web.Authenticate.OAuth as OA



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

getAuthorizeUrl :: MonadIO m => N.Manager -> OAuth -> m String
getAuthorizeUrl manager oauth = do
  cred <- OA.getTemporaryCredential oauth manager
  return $ OA.authorizeUrl oauth cred

getDebugAuthorizeUrl :: MonadIO m => OAuth -> m String
getDebugAuthorizeUrl oauth = do
  manager <- liftIO $ N.newManager N.defaultManagerSettings
  getAuthorizeUrl manager oauth

getAuthorizeUrlM :: (MonadIO m, MonadReader r m, N.HasHttpManager r) => OAuth -> m String
getAuthorizeUrlM oauth = do
  manager <- asks N.getHttpManager
  cred <- OA.getTemporaryCredential oauth manager
  return $ OA.authorizeUrl oauth cred
