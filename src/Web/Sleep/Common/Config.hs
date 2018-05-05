{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Common.Config (
  Config(..),
  HasConfig(..),
  MonadConfig,
  defaultIOConfig,
  makeMockConfig,
  ) where



-- imports

import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy             as LB
import qualified Network.HTTP.Client              as N
import qualified Web.Authenticate.OAuth           as OA

import           Web.Sleep.Common.Helpers.Base
import           Web.Sleep.Common.Helpers.Request



-- config struct

data Config m = Config
  { networkManager :: N.Manager
  , requestSign    :: OA.OAuth -> OA.Credential -> N.Request -> m N.Request
  , networkSend    :: N.Request -> m (N.Response LB.ByteString)
  }

class HasConfig a where
  type ConfigBase a :: * -> *
  getConfig :: a -> Config (ConfigBase a)

instance HasConfig (Config m) where
  type ConfigBase (Config m) = m
  getConfig = id

instance N.HasHttpManager (Config m) where
  getHttpManager = networkManager

type MonadConfig r m = (MonadReader r m, MonadBase m, HasConfig r, Base m ~ ConfigBase r)



-- default configs

defaultIOConfig :: N.Manager -> Config IO
defaultIOConfig m = Config m sign send
  where sign = OA.signOAuth
        send = flip N.httpLbs m

makeMockConfig :: [(String, String)] -> Config Identity
makeMockConfig reqMap = Config manager sign send
  where sign _ (OA.Credential creds) = return . appendParams creds
        send req = return $ maybe undefined undefined $ lookup (show req) reqMap
        manager = error "tried to access mock network manager"
