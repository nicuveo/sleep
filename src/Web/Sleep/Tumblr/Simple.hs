{- |
Module: Web.Sleep.Tumblr.Simple

A collection of helpers aiming at providing a good enough out of the box
experience with the Tumblr API.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}



-- module

module Web.Sleep.Tumblr.Simple (
  NetworkConfig(..),
  TumblrK(TumblrK),
  TumblrA(TumblrA),
  callTumblrK,
  callKE,
  callKT,
  callK,
  callTumblrA,
  callAE,
  callAT,
  callA,
  withAPIKey,
  withOAuth,
  getSimpleAuthCred,
  checkAuthValidity,
  defaultManager,
  defaultIONetworkConfig,
  makeMockNetworkConfig,
  cliURLCallback,
  ) where



-- imports

import           Control.Exception.Safe
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.ByteString.Char8   (pack)
import qualified Data.ByteString.Lazy    as LB
import qualified Network.HTTP.Client     as N
import qualified Network.HTTP.Client.TLS as N
import           System.IO
import qualified Web.Authenticate.OAuth  as OA

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr



-- context data

data NetworkConfig m = NetworkConfig
  { networkSend :: N.Request -> m (N.Response LB.ByteString)
  , httpManager :: N.Manager
  }

data TumblrK m = TumblrK
  { networkConfig :: NetworkConfig m
  , apiKey        :: AppKey
  }

data TumblrA m = TumblrA
  { tumblrK :: TumblrK m
  , _sign   :: OAuthFunction m
  }



-- access to context

class HasTumblrK a m where
  getTumblrK :: a -> TumblrK m

class HasTumblrA a m where
  getTumblrA :: a -> TumblrA m


instance N.HasHttpManager (TumblrK m)   where getHttpManager = httpManager . networkConfig
instance HasTumblrK       (TumblrK m) m where getTumblrK = id

instance N.HasHttpManager (TumblrA m)   where getHttpManager = httpManager . networkConfig . tumblrK
instance HasTumblrK       (TumblrA m) m where getTumblrK = tumblrK
instance HasTumblrA       (TumblrA m) m where getTumblrA = id

type MonadTumblrK r b m = (MonadReader r m, MonadBase b m, HasTumblrK r b)
type MonadTumblrA r b m = (MonadReader r m, MonadBase b m, HasTumblrA r b)



-- calling the network

type MonadQueryK r b m i o = (MonadTumblrK r b m, Decode i o, APIKeyCommand i)
type MonadQueryA r b m i o = (MonadTumblrA r b m, Decode i o, OAuthCommand  i)

callTumblrK :: (Monad m, Decode i o, APIKeyCommand i) => TumblrK m -> Query i -> m (Either Error o)
callTumblrK k q = fmap (decode q) $ networkSend (networkConfig k) $ mkAPIKeyRequest (apiKey k) q

callKE :: (MonadQueryK r b m i o, MonadError Error m) => Query i -> m o
callKT :: (MonadQueryK r b m i o, MonadThrow m)       => Query i -> m o
callK  ::  MonadQueryK r b m i o                      => Query i -> m (Either Error o)
callKE = either throwError return <=< callK
callKT = either throw      return <=< callK
callK q = do
  k <- asks getTumblrK
  liftBase $ callTumblrK k q

callTumblrA :: (Monad m, Decode i o, OAuthCommand i) => TumblrA m -> Query i -> m (Either Error o)
callTumblrA a q = fmap (decode q) . networkSend nc =<< mkOAuthRequest sf ak q
  where TumblrA (TumblrK nc ak) sf = a

callAE :: (MonadQueryA r b m i o, MonadError Error m) => Query i -> m o
callAT :: (MonadQueryA r b m i o, MonadThrow m)       => Query i -> m o
callA  ::  MonadQueryA r b m i o                      => Query i -> m (Either Error o)
callAE = either throwError return <=< callA
callAT = either throw      return <=< callA
callA q = do
  a <- asks getTumblrA
  liftBase $ callTumblrA a q



-- simple reader wrappers

withAPIKey :: NetworkConfig b -> AppKey -> ReaderT (TumblrK b) m a -> m a
withAPIKey nc ak = with $ TumblrK nc ak

withOAuth :: NetworkConfig b -> AppKey -> OAuthFunction b -> ReaderT (TumblrA b) m a -> m a
withOAuth nc ak sf = with $ TumblrA (TumblrK nc ak) sf



-- auth simple helpers

type URLCallback m = String -> m String

{- | A very simple wrapper around "Web.Authenticate.OAuth"'s functions. The
   'URLCallback' paranameter is a callback that expects the authentication URL
   as an argument and returns the validation token as a result. Takes the
   original Tumblr 'OAuth' as an argument, returns the credentials after
   authenticating the user.
-}
getSimpleAuthCred :: MonadIO m => URLCallback m -> N.Manager -> OAuth -> m AuthCred
getSimpleAuthCred callback manager oauth = do
  tempCred <- OA.getTemporaryCredential oauth manager
  verifier <- callback $ OA.authorizeUrl oauth tempCred
  let newCred = OA.injectVerifier (pack verifier) tempCred
  cred     <- OA.getAccessToken oauth newCred manager
  return (oauth, cred)

checkAuthValidity :: MonadTumblrA r b m => m Bool
checkAuthValidity = do
  res <- callA $ getBlogInfo "test.tumblr.com"
  return $ either (== undefined) (const False) res



-- simple network usage

defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultIONetworkConfig :: N.Manager -> NetworkConfig IO
defaultIONetworkConfig m = NetworkConfig send m
  where send = flip N.httpLbs m

makeMockNetworkConfig :: [(String, String)] -> NetworkConfig Identity
makeMockNetworkConfig reqMap = NetworkConfig send m
  where send req = return $ maybe undefined undefined $ lookup (show req) reqMap
        m = error "tried to access mock manager"

cliURLCallback :: URLCallback IO
cliURLCallback url = do
  putStrLn $ "Please authorize this tool at: " ++ url
  putStr "Please input the validation token: "
  hFlush stdout
  getLine
