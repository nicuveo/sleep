module Web.Sleep.Tumblr.Monad where

import "sleep-core" Prelude

import Data.ByteString       qualified as BS
import Network.HTTP.Client   qualified as Network

import Web.Sleep.Core.Error
import Web.Sleep.Tumblr.Auth

type UserAgent = Text

-- | Tumblr capablity.
--
-- All network operations are performed through a 'MonadTumblr' instance,
-- allowing users to decide how network operations are performed. No instance is
-- provided, but the @Simple@ module provides a basic implementation.
class MonadError Error m => MonadTumblr m where
  sendRequest :: Network.Request -> m (Network.Response BS.ByteString)
  getUserAgent :: m UserAgent

class MonadTumblr m => MonadAPIKeyTumblr m where
  getAPIKey :: m APIKey

class MonadAPIKeyTumblr m => MonadOAuthTumblr m where
  signRequest :: Network.Request -> m Network.Request
