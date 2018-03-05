{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}



-- module

module Web.Sleep.Tumblr.Response (
  EnvelopeFromJSON,
  getResponse,
  getResponseT,
  getResponseE) where



-- imports

import           Control.Arrow
import           Control.Exception.Safe
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy
import qualified Data.Vector            as V

import           Web.Sleep.Tumblr.Error



-- exported functions

getResponse :: (EnvelopeFromJSON a) => RawData -> Either Error a
getResponse = join . fmap getRes . left _jsonError . eitherDecode'

getResponseT :: (EnvelopeFromJSON a, MonadThrow m) => RawData -> m a
getResponseT = either throw return . getResponse

getResponseE :: (EnvelopeFromJSON a, MonadError Error m) => RawData -> m a
getResponseE = either throwError return . getResponse



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

instance EnvelopeFromJSON a => FromJSON (Envelope a) where
  parseJSON = withObject "envelope" $ \o -> do
    Meta status msg <- o .: "meta"
    if status == 200
    then toEnvelope o
    else do
      let result s = return $ Envelope $ Left $ ServerError status $ msg ++ s
      maybeErrors <- o .:? "errors"
      case maybeErrors of
        Nothing -> result ""
        Just es -> do
          detail <- withObject "error" (.: "detail") =<< withArray "errors" (return . V.head) es
          result $ " (" ++ detail ++ ")"



-- special case for ()

class FromJSON a => EnvelopeFromJSON a where
  toEnvelope :: Object -> Parser (Envelope a)

instance FromJSON a => EnvelopeFromJSON a where
  toEnvelope o = Envelope . Right <$> o .: "response"

instance {-# INCOHERENT #-} EnvelopeFromJSON () where
  toEnvelope _ = return $ Envelope $ Right ()
