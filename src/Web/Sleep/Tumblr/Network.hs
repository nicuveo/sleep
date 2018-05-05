{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Network (
  module Web.Sleep.Common.Network,
  call,
  callT,
  callE,
  MonadTumblrCall,
  MonadTumblrCallT,
  MonadTumblrCallE,
  Decode(..),
  decodeJSON,
  ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                    (FromJSON)
import           Data.ByteString.Lazy
import qualified Network.HTTP.Client       as N

import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- putting together network and parsing

call  :: (MonadNetwork c m, ToRequest q m, Decode (RequestResult q))
          => q -> m (Either Error (RequestResult q))
callT :: (MonadNetwork c m, ToRequest q m, Decode (RequestResult q), MonadThrow m)
          => q -> m (RequestResult q)
callE :: (MonadNetwork c m, ToRequest q m, Decode (RequestResult q), MonadError Error m)
          => q -> m (RequestResult q)
call  = doCall >=>                   return . decode
callT = doCall >=> either throw      return . decode
callE = doCall >=> either throwError return . decode

type MonadTumblrCall  c m = (MonadNetwork c m, MonadSign m)
type MonadTumblrCallT c m = (MonadNetwork c m, MonadSign m, MonadThrow m)
type MonadTumblrCallE c m = (MonadNetwork c m, MonadSign m, MonadError Error m)



-- decode class

class Decode a where
  decode :: N.Response ByteString -> Either Error a
  default decode :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
  decode = decodeJSON

instance Decode ()

decodeJSON :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
decodeJSON = getResponse . N.responseBody



-- helper

doCall :: (MonadNetwork c m, ToRequest q m) => q -> m (N.Response ByteString)
doCall q = do
  r <- toRequest q
  c <- ask
  send c r
