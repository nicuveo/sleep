{-# OPTIONS_GHC -Wno-partial-fields #-}

module Web.Sleep.Core.Error where

import "this" Prelude

import Control.Exception.Safe
import Text.Printf


--------------------------------------------------------------------------------
-- * Generic error type

data Error
  = HttpError { errorMsg :: Text }
  | NetworkError { errorCode :: Int }
  | ServerError { errorCode :: Int, errorMsg :: Text }
  | JSONError { errorMsg :: Text }
  deriving (Eq)

instance Show Error where
  show = \case
    HttpError msg ->
      printf "http error: %s" msg
    NetworkError code ->
      printf "network error: %d" code
    ServerError code msg ->
      printf "server error: %s (code %d)" msg code
    JSONError msg ->
      printf "serialization error: %s" msg

instance Exception Error
