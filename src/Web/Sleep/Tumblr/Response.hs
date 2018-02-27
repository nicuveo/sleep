{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}



-- module

module Web.Sleep.Tumblr.Response (getResponse, getResponseT, getResponseE) where



-- imports

import           Control.Arrow
import           Control.Exception.Safe
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Vector            as V

import           Web.Sleep.Tumblr.Error



-- exported functions

getResponse :: FromJSON a => RawData -> Either Error a
getResponse = join . fmap getRes . left jsonError . eitherDecode'

getResponseT :: (MonadThrow m, FromJSON a) => RawData -> m a
getResponseT = either throw return . getResponse

getResponseE :: (MonadError Error m, FromJSON a) => RawData -> m a
getResponseE = either throwError return . getResponse



-- internal types

type RawData = ByteString

data Meta = Meta Int String deriving Show

newtype Envelope a = Envelope { getRes :: Either Error a } deriving Show



-- internal functions

jsonError :: String -> Error
jsonError = ClientError 1 -- FIXME



-- instances

instance FromJSON Meta where
  parseJSON = withObject "envelope meta" $ \o -> do
    s <- o .: "status"
    m <- o .: "msg"
    return $ Meta s m

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = withObject "envelope" $ \o -> do
    Meta status msg <- o .: "meta"
    if status == 200
    then Envelope . Right <$> o .: "response"
    else do
      detail <- o .: "errors"
                >>= withArray "errors" (return . V.head)
                >>= withObject "error" (.: "detail")
      return $ Envelope $ Left $ ServerError status $ msg ++ " (" ++ detail ++ ")"
