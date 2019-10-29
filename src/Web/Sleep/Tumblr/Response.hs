{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}



-- module

module Web.Sleep.Tumblr.Response (
  Envelope,
  decodeJSON,
  ) where



-- imports

import           Control.Arrow
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy
import qualified Data.Vector            as V
import qualified Network.HTTP.Client    as N

import           Web.Sleep.Tumblr.Error



-- exported types

newtype Envelope a = Envelope { getRes :: Either Error a } deriving Show

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = parseEnvelope

instance {-# OVERLAPS #-} FromJSON (Envelope ()) where
  parseJSON = parseEnvelopeWith $ \_ -> return $ Envelope $ Right ()



-- exported functions

decodeJSON :: FromJSON (Envelope a) => N.Response ByteString -> Either Error a
decodeJSON = getResponse . N.responseBody



-- internal types

data Meta = Meta Int String deriving Show

instance FromJSON Meta where
  parseJSON = withObject "envelope meta" $ \o -> do
    s <- o .: "status"
    m <- o .: "msg"
    return $ Meta s m



-- local helpers

jsonError :: String -> Error
jsonError = ClientError 1 -- FIXME

getResponse :: (FromJSON (Envelope a)) => ByteString -> Either Error a
getResponse = getRes <=< left jsonError . eitherDecode'

parseEnvelope :: FromJSON a => Value -> Parser (Envelope a)
parseEnvelope = parseEnvelopeWith $ \o -> Envelope . Right <$> o .: "response"

parseEnvelopeWith :: (Object -> Parser (Envelope a)) -> Value -> Parser (Envelope a)
parseEnvelopeWith toEnvelope = withObject "envelope" $ \o -> do
    Meta status msg <- o .: "meta"
    if status >= 200 && status < 300
    then toEnvelope o
    else do
      let result s = return $ Envelope $ Left $ ServerError status $ msg ++ s
      maybeErrors <- o .:? "errors"
      case maybeErrors of
        Nothing -> result ""
        Just es -> do
          detail <- withObject "error" (.: "detail") =<< withArray "errors" (return . V.head) es
          result $ " (" ++ detail ++ ")"
