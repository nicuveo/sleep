module Web.Sleep.Tumblr.Response
  ( Envelope
  , unwrap
  ) where

import "sleep-core" Prelude

import Data.Aeson
import Data.Aeson.Types
import Data.Text            qualified as Text
import Data.Vector          qualified as Vector

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
  Meta status msg <- o .: "meta"
  if status >= 200 && status < 300
  then toEnvelope o
  else do
    o .:? "errors" >>= \case
      Nothing -> failWith status msg
      Just es -> displayErrors status msg es
  where
    failWith status errorMsg =
      pure $ Envelope $ Left $ ServerError status $ Text.pack errorMsg
    displayErrors status msg = withArray "errors" \errs -> do
      errorList <- traverse displayError $ Vector.toList errs
      failWith status $ msg ++ "\n" ++ unlines errorList
    displayError = withObject "error" \err -> do
      msg <- err .: "detail"
      pure $ "  - " ++ msg
