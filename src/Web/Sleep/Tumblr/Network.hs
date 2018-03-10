{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Network (
  call,
  callT,
  callE,
  ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Lazy

import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- putting together network and parsing

call  :: (MonadNetwork c m, ToRequest q m, EnvelopeFromJSON (RequestResult q))
          => q -> m (Either Error (RequestResult q))
callT :: (MonadNetwork c m, ToRequest q m, EnvelopeFromJSON (RequestResult q), MonadThrow m)
          => q -> m (RequestResult q)
callE :: (MonadNetwork c m, ToRequest q m, EnvelopeFromJSON (RequestResult q), MonadError Error m)
          => q -> m (RequestResult q)
call  = doCall >=> return . getResponse
callT = doCall >=> getResponseT
callE = doCall >=> getResponseE



-- helper

doCall :: (MonadNetwork c m, ToRequest q m) => q -> m ByteString
doCall q = do
  r <- toRequest q
  c <- ask
  send c r
