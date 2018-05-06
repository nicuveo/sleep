-- module

module Web.Sleep.Tumblr.Response (
  RawData,
  Envelope,
  getResponse,
  parseEnvelope,
  ) where



-- imports

import           Control.Arrow
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy
import qualified Data.Vector            as V

import           Web.Sleep.Tumblr.Error



-- exported functions

getResponse :: (FromJSON (Envelope a)) => RawData -> Either Error a
getResponse = join . fmap getRes . left _jsonError . eitherDecode'



-- internal types

type RawData = ByteString

data Meta = Meta Int String deriving Show

newtype Envelope a = Envelope { getRes :: Either Error a } deriving Show



-- internal functions

_jsonError :: String -> Error
_jsonError = ClientError 1 -- FIXME



-- instances

instance FromJSON Meta where
  parseJSON = withObject "envelope meta" $ \o -> do
    s <- o .: "status"
    m <- o .: "msg"
    return $ Meta s m

instance FromJSON (Envelope ()) where
  parseJSON = parseEnvelopeWith $ \_ -> return $ Envelope $ Right ()



-- local helpers

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
