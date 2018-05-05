{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-

Network related helper types and functions.

-}



-- module

module Web.Sleep.Common.Network (
  QMethod(..),
  ToRequest(..),
  defaultManager,
  ) where



-- imports

import           Control.Monad.Cont
import qualified Network.HTTP.Client     as N
import qualified Network.HTTP.Client.TLS as N

import           Web.Sleep.Common.Config



-- request helpers

data QMethod = QGet | QPost deriving (Show, Eq)

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: MonadConfig r m => a -> m N.Request



-- simple network functions

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings
