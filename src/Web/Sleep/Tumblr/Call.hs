{-# LANGUAGE DefaultSignatures #-}



-- module

module Web.Sleep.Tumblr.Call (
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

import           Web.Sleep.Libs.Base

import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- putting together network and parsing

call  :: (MonadNetwork r m, ToRequest q m, Decode (RequestResult q))                     => m q -> m (Either Error (RequestResult q))
callT :: (MonadNetwork r m, ToRequest q m, Decode (RequestResult q), MonadThrow m)       => m q -> m (RequestResult q)
callE :: (MonadNetwork r m, ToRequest q m, Decode (RequestResult q), MonadError Error m) => m q -> m (RequestResult q)
call  q =                            decode <$>(doCall =<< q)
callT q = either throw      return . decode =<< doCall =<< q
callE q = either throwError return . decode =<< doCall =<< q



-- decode class

class Decode a where
  decode :: N.Response ByteString -> Either Error a
  default decode :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
  decode = decodeJSON

instance Decode ()

decodeJSON :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
decodeJSON = getResponse . N.responseBody



-- helper

doCall :: (MonadNetwork r m, ToRequest q m) => q -> m (N.Response ByteString)
doCall q = do
  config  <- asks getNetworkConfig
  request <- toRequest q
  liftBase $ networkSend config request
