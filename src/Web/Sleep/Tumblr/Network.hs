{-# LANGUAGE DefaultSignatures #-}



-- module

module Web.Sleep.Tumblr.Network (
  module Web.Sleep.Common.Config,
  module Web.Sleep.Common.Network,
  call,
  callT,
  callE,
  Decode(..),
  decodeJSON,
  ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                    (FromJSON)
import           Data.ByteString.Lazy
import qualified Network.HTTP.Client           as N

import           Web.Sleep.Common.Config
import           Web.Sleep.Common.Helpers.Base
import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- putting together network and parsing

call  :: (MonadConfig r m, ToRequest q m, Decode (RequestResult q))                     => q -> m (Either Error (RequestResult q))
callT :: (MonadConfig r m, ToRequest q m, Decode (RequestResult q), MonadThrow m)       => q -> m (RequestResult q)
callE :: (MonadConfig r m, ToRequest q m, Decode (RequestResult q), MonadError Error m) => q -> m (RequestResult q)
call  q =                            decode <$> doCall q
callT q = either throw      return . decode =<< doCall q
callE q = either throwError return . decode =<< doCall q



-- decode class

class Decode a where
  decode :: N.Response ByteString -> Either Error a
  default decode :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
  decode = decodeJSON

instance Decode ()

decodeJSON :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
decodeJSON = getResponse . N.responseBody



-- helper

doCall :: (MonadConfig r m, ToRequest q m) => q -> m (N.Response ByteString)
doCall q = do
  config  <- asks getConfig
  request <- toRequest q
  liftBase $ networkSend config request
