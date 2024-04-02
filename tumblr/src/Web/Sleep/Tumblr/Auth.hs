module Web.Sleep.Tumblr.Auth where

import "sleep-core" Prelude

import Data.ByteString        qualified as B
import Network.HTTP.Client    qualified as N

import Web.Sleep.Core.Request


type APIKey      = B.ByteString
type AppSecret   = B.ByteString
type BearerToken = B.ByteString


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
