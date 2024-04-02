module Web.Sleep.Core.Command where

import "this" Prelude

import Data.ByteString      qualified as B
import Network.HTTP.Client  qualified as N

import Web.Sleep.Core.Error


data Command a = Command
  { request :: N.Request
  , decode  :: B.ByteString -> Either Error a
  } deriving (Functor)
