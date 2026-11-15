{-# LANGUAGE TemplateHaskell #-}

module Web.Sleep.Tumblr.Simple where

import "sleep-core" Prelude

import Control.Exception
import Control.Lens           (view)
import Control.Lens.TH
import Data.ByteString        qualified as BS
import Data.Text              qualified as Text
import Data.Text.Encoding     (encodeUtf8)
import Network.HTTP.Client    qualified as Network
import Web.Authenticate.OAuth qualified as OAuth

import Web.Sleep.Core.Error
import Web.Sleep.Tumblr.Auth
import Web.Sleep.Tumblr.Monad


--------------------------------------------------------------------------------
-- Simple functions

getSimpleTemporaryCredentials
  :: MonadIO m
  => Network.Manager
  -> OAuthConfig
  -> m (Credentials, CallbackURL)
getSimpleTemporaryCredentials manager oauth = do
  tempCreds <- OAuth.getTemporaryCredential oauth manager
  let callbackURL = OAuth.authorizeUrl oauth tempCreds
  pure (tempCreds, callbackURL)

getSimpleCredentials
  :: MonadIO m
  => Network.Manager
  -> OAuthConfig
  -> Credentials
  -> Verifier
  -> m Credentials
getSimpleCredentials manager oauth tempCreds verifier = do
  OAuth.getAccessToken oauth (OAuth.injectVerifier (encodeUtf8 verifier) tempCreds) manager

simpleRequest
  :: (MonadIO m, MonadError Error m)
  => Network.Manager
  -> Network.Request
  -> m (Network.Response BS.ByteString)
simpleRequest manager request = do
  result <- liftIO do
    go `catch` \(e :: Network.HttpException) ->
      pure $ Left $ HttpError $ Text.pack $ displayException e
  result `onLeft` throwError
  where
    go = do
      response <- Network.httpLbs request manager
      pure $ Right $ fmap BS.toStrict response


--------------------------------------------------------------------------------
-- Simple monad

data SimpleTumblrInfo = SimpleTumblrInfo
  { _stiNetworkManager :: Network.Manager
  , _stiUserAgent      :: UserAgent
  , _stiAPIKey         :: APIKey
  }

makeLenses ''SimpleTumblrInfo

newtype SimpleTumblr a = SimpleTumblr (ExceptT Error (ReaderT SimpleTumblrInfo IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)

instance MonadTumblr SimpleTumblr where
  sendRequest req = SimpleTumblr do
    manager <- view stiNetworkManager
    simpleRequest manager req
  getUserAgent = SimpleTumblr do
    view stiUserAgent

instance MonadAPIKeyTumblr SimpleTumblr where
  getAPIKey = SimpleTumblr do
    view stiAPIKey

runSimpleTumblr
  :: Network.Manager
  -> UserAgent
  -> APIKey
  -> SimpleTumblr a
  -> IO (Either Error a)
runSimpleTumblr manager userAgent apiKey (SimpleTumblr action) = do
  runReaderT (runExceptT action) $ SimpleTumblrInfo manager userAgent apiKey
