module Web.Sleep.Tumblr.Auth where

import "sleep-core" Prelude

import Data.Text.Encoding     (encodeUtf8)
import Web.Authenticate.OAuth qualified as OAuth

type APIKey      = Text
type AppSecret   = Text
type CallbackURL = String
type Verifier    = Text
type OAuthConfig = OAuth.OAuth
type Credentials = OAuth.Credential

tumblrOAuth :: APIKey -> AppSecret -> OAuthConfig
tumblrOAuth key secret =
  OAuth.newOAuth
    { OAuth.oauthServerName      = "tumblr"
    , OAuth.oauthRequestUri      = "https://www.tumblr.com/oauth/request_token"
    , OAuth.oauthAuthorizeUri    = "https://www.tumblr.com/oauth/authorize"
    , OAuth.oauthAccessTokenUri  = "https://www.tumblr.com/oauth/access_token"
    , OAuth.oauthVersion         = OAuth.OAuth10a
    , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
    , OAuth.oauthConsumerKey     = encodeUtf8 key
    , OAuth.oauthConsumerSecret  = encodeUtf8 secret
    }

{-

data AuthorizationLevel n k t
  = ALNone  n
  | ALKey   k APIKey
  | ALToken t BearerToken

type NoAuthRequired = AuthorizationLevel ()   ()   ()
type APIKeyRequired = AuthorizationLevel Void ()   ()
type TokenRequired  = AuthorizationLevel Void Void ()

noAuth :: AuthorizationLevel () k t
noAuth = ALNone ()

apiKey :: APIKey -> AuthorizationLevel n () t
apiKey = ALKey ()

token :: BearerToken -> AuthorizationLevel n k ()
token = ALToken ()


signRequest :: AuthorizationLevel n k t -> N.Request -> N.Request
signRequest = \case
  ALNone  _   -> id
  ALKey   _ k -> addParameter "api_key" k
  ALToken _ t -> addHeader "Authorization" (B.concat ["Bearer ", t])

-}
