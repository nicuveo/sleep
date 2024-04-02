module Web.Sleep.Tumblr.Response
  ( Envelope
  , unwrap
  ) where

import "sleep-core" Prelude

import Data.Aeson
import Data.Aeson.Types

import Web.Sleep.Core.Error


--------------------------------------------------------------------------------
-- Envelope type

newtype Envelope a = Envelope (Either Error a)

instance {-# OVERLAPPING #-} FromJSON (Envelope ()) where
  parseJSON = parseEnvelopeWith $ const $ pure $ Envelope $ Right ()

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = parseEnvelopeWith \o -> Envelope . Right <$> o .: "response"


unwrap :: Envelope a -> Either Error a
unwrap (Envelope r) = r


--------------------------------------------------------------------------------
-- Internal helpers

data Meta = Meta Int String deriving Show

instance FromJSON Meta where
  parseJSON = withObject "envelope meta" \o -> Meta
    <$> (o .: "status")
    <*> (o .: "msg")

parseEnvelopeWith :: (Object -> Parser (Envelope a)) -> Value -> Parser (Envelope a)
parseEnvelopeWith toEnvelope = withObject "envelope" \o -> do
  Meta status _msg <- o .: "meta"
  if status >= 200 && status < 300
  then toEnvelope o
  else undefined -- TODO: handle errors

{-
    let result s = return $ Envelope $ Left $ ServerError status $ msg ++ s
    maybeErrors <- o .:? "errors"
    case maybeErrors of
      Nothing -> result ""
      Just es -> do
        detail <- withObject "error" (.: "detail") =<< withArray "errors" (return . V.head) es
        result $ " (" ++ detail ++ ")"
-}
