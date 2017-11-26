{-# LANGUAGE ConstraintKinds #-}

{-

Network related helper types and functions.

-}



-- module

module Web.Sleep.Common.Network (
  MonadManager,
  ) where



-- imports

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Network.HTTP.Client as N



-- exported types

type MonadManager r m = (MonadIO m, MonadReader r m, N.HasHttpManager r)
