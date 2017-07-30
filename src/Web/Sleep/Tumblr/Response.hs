{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}



-- module

module Web.Sleep.Tumblr.Response (getResponse, getResponseM) where



-- imports

import           Control.Arrow
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy

import           Web.Sleep.Tumblr.Error



-- exported functions

getResponse :: FromJSON a => RawData -> Either Error a
getResponse rd = do
  env <- left jsonError $ eitherDecode' rd
  if isOk env
    then Right $ envResp env
    else Left $ metaToError $ envMeta env
  where isOk :: Envelope a -> Bool
        isOk (metaStatus . envMeta -> status) = status == 200


getResponseM :: (MonadError Error m, FromJSON a) => RawData -> m a
getResponseM = either throwError return . getResponse



-- internal types

type RawData = ByteString

data Meta = Meta { metaStatus :: Int
                 , metaMsg    :: String
                 } deriving (Show)

data Envelope a = Envelope { envMeta :: Meta
                           , envResp :: a
                           } deriving (Show)



-- internal functions

metaToError :: Meta -> Error
metaToError (Meta c m) = ServerError c m

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
    m <- o .: "meta"
    r <- o .: "response"
    return $ Envelope m r
