{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}



-- module

module Web.Sleep.Tumblr.Response (
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

getResponse :: (FromJSONResponse Envelope a, FromJSON a) => RawData -> Either Error a
getResponse = join . fmap getRes . left _jsonError . eitherDecode'

getResponseT :: (MonadThrow m, FromJSON a) => RawData -> m a
getResponseT = either throw return . getResponse

getResponseE :: (MonadError Error m, FromJSON a) => RawData -> m a
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

instance (FromJSON a, FromJSONResponse Envelope a) => FromJSON (Envelope a) where
  parseJSON = withObject "envelope" $ \o -> do
    Meta status msg <- o .: "meta"
    if status == 200
    then fromObject o
    else do
      detail <- o .: "errors"
                >>= withArray "errors" (return . V.head)
                >>= withObject "error" (.: "detail")
      return $ Envelope $ Left $ ServerError status $ msg ++ " (" ++ detail ++ ")"



-- special case for ()

class FromJSONResponse e a where
  fromObject :: Object -> Parser (e a)

instance FromJSON a => FromJSONResponse Envelope a where
  fromObject o = Envelope . Right <$> o .: "response"

instance {-# INCOHERENT #-} FromJSONResponse Envelope () where
  fromObject _ = return $ Envelope $ Right ()
