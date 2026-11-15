module Web.Sleep.Tumblr.Request
  ( request
  , apiRequest
  , authenticatedRequest
  , (/.)
  , (&.)
  , (&?.)
  , (@.)
  ) where

import "sleep-core" Prelude

import Data.Aeson
import Data.ByteString           qualified as BS
import Data.Text                 qualified as Text
import Data.Text.Encoding        (encodeUtf8)
import Network.HTTP.Client       qualified as Network
import Network.HTTP.Types        (Method)
import Network.HTTP.Types        qualified as Network
import Network.HTTP.Types.Header (hContentType, hUserAgent)
import Web.HttpApiData           (ToHttpApiData)

import Web.Sleep.Core.Error
import Web.Sleep.Core.Request
import Web.Sleep.Tumblr.Monad
import Web.Sleep.Tumblr.Response


--------------------------------------------------------------------------------
-- Public API

request
  :: (FromJSON (Envelope a), MonadTumblr m)
  => Request
  -> m a
request (Request req) = do
  finalizeRequest req
    >>= sendRequest
    >>= handleResponse

apiRequest
  :: (FromJSON (Envelope a), MonadAPIKeyTumblr m)
  => Request
  -> m a
apiRequest (Request req) = do
  addKey req
    >>= finalizeRequest
    >>= sendRequest
    >>= handleResponse

authenticatedRequest
  :: (FromJSON (Envelope a), MonadOAuthTumblr m)
  => Request
  -> m a
authenticatedRequest (Request req) = do
  addKey req
    >>= finalizeRequest
    >>= signRequest
    >>= sendRequest
    >>= handleResponse


--------------------------------------------------------------------------------
-- Opaque request type

newtype Request = Request Network.Request

(/.) :: Method -> Text -> Request
method /. path =
  Request $
    Network.defaultRequest
      { Network.path   = encodeUtf8 path
      , Network.method = method
      , Network.host   = "api.tumblr.com"
      }
infixl 1 /.

(&.) :: (ToHttpApiData s) => Request -> (s, Maybe s) -> Request
Request req &. (name, value) = Request $ addParameter name value req
infixl 1 &.

(&?.) :: (ToHttpApiData s) => Request -> Maybe (s, s) -> Request
(&?.) (Request req) = \case
  Nothing ->
    Request req
  Just (name, value) ->
    Request $ addParameter name (Just value) req
infixl 1 &?.

(@.) :: Request -> BS.ByteString -> Request
Request req @. body = Request $ setBody body req
infixl 1 @.


--------------------------------------------------------------------------------
-- Internal helpers

addKey
  :: (MonadAPIKeyTumblr m)
  => Network.Request
  -> m Network.Request
addKey req = do
  apiKey <- getAPIKey
  pure $ req
    & addParameter "api_key" (Just apiKey)

finalizeRequest
  :: (MonadTumblr m)
  => Network.Request
  -> m Network.Request
finalizeRequest req = do
  userAgent <- getUserAgent
  pure $ req
    & addHeader hContentType ("application/json" :: Text)
    & addHeader hUserAgent userAgent

handleResponse
  :: (FromJSON (Envelope a), MonadError Error m)
  => Network.Response BS.ByteString
  -> m a
handleResponse response = do
  case Network.statusCode $ Network.responseStatus response of
    200 -> do
      case eitherDecode (BS.fromStrict $ Network.responseBody response) of
        Left msg  -> throwError $ JSONError $ Text.pack msg
        Right env -> unwrap env `onLeft` throwError
    errorCode -> do
      throwError $ NetworkError errorCode
