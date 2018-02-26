{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-

Network related helper types and functions.

-}



-- module

module Web.Sleep.Common.Network (
  QMethod(..),
  MonadManager,
  ToRequest(..),
  ) where



-- imports

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Network.HTTP.Client    as N



-- exported types

data QMethod = QGet | QPost

type MonadManager r m = (MonadIO m, MonadReader r m, N.HasHttpManager r)



-- exported classes

class Monad m => ToRequest a m where
  type RequestResult a :: *
  toRequest :: a -> m N.Request
