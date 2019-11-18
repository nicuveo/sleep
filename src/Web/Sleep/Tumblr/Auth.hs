-- module

module Web.Sleep.Tumblr.Auth (
  -- exported types
  APIKey,
  AppSecret,
  OAuth,
  Credential,
  AuthCred,
  -- exported functions
  tumblrOAuth,
  ) where



-- imports

import           Data.ByteString
import qualified Web.Authenticate.OAuth as OA



-- type aliases

type APIKey     = ByteString
type AppSecret  = ByteString
type OAuth      = OA.OAuth
type Credential = OA.Credential
type AuthCred   = (OAuth, Credential)



-- exported functions

tumblrOAuth :: APIKey -> AppSecret -> OAuth
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
