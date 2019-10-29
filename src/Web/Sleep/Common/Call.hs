{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}



-- module

module Web.Sleep.Common.Call (
  module Web.Sleep.Common.Network,
  call,
  callT,
  callE,
  callM,
  callMT,
  callME,
  ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Base
import           Control.Monad.Except

import           Web.Sleep.Common.Network



-- network call functions

type Throwable e m = (MonadThrow m, Exception e)

call   :: (Operation f t e, Monad m)         => NetworkConfig m -> f -> m (Either e t)
callT  :: (Operation f t e, Throwable  e m)  => NetworkConfig m -> f -> m t
callE  :: (Operation f t e, MonadError e m)  => NetworkConfig m -> f -> m t
callM  :: (Operation f t e, MonadNetwork r b m)                 => f -> m (Either e t)
callMT :: (Operation f t e, MonadNetwork r b m, Throwable  e m) => f -> m t
callME :: (Operation f t e, MonadNetwork r b m, MonadError e m) => f -> m t
call   = doCall return
callT  = doCall $ either throw return
callE  = doCall $ either throwError return
callM  = doCallM return
callMT = doCallM $ either throw return
callME = doCallM $ either throwError return




-- helper

doCall :: (Operation f t e, Monad m) => (Either e t -> m a) -> NetworkConfig m -> f -> m a
doCall p c f = p . fromResponse f =<< networkSend c =<< toRequest f c

doCallM :: (Operation f t e, MonadNetwork r b m) => (Either e t -> m a) -> f -> m a
doCallM p f = do
  c <- getNetworkConfig
  r <- liftBase $ doCall return c f
  p r
