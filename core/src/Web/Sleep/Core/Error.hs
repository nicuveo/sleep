module Web.Sleep.Core.Error where

import "this" Prelude

import Control.Exception.Safe
import Text.Printf


--------------------------------------------------------------------------------
-- * Generic error type

data Error
  = ServerError { errorCode :: Int, errorMsg :: Text }
  | ClientError { errorCode :: Int, errorMsg :: Text }
  deriving (Eq)

instance Show Error where
  show (ServerError c m) = printf "server error: %s (code %d)" m c
  show (ClientError c m) = printf "client error: %s (code %d)" m c

instance Exception Error
