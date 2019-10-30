{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- module

module Web.Sleep.Common.Network where



{-
defaultManager :: MonadIO m => m N.Manager
defaultManager = liftIO $ N.newManager N.tlsManagerSettings

defaultIONetworkConfig :: N.Manager -> NetworkConfig IO
defaultIONetworkConfig m = NetworkConfig m sign send
  where sign = OA.signOAuth
        send = flip N.httpLbs m

makeMockNetworkConfig :: [(String, String)] -> NetworkConfig Identity
makeMockNetworkConfig reqMap = NetworkConfig manager sign send
  where sign _ (OA.Credential creds) = return . appendParams creds
        send req = return $ maybe undefined undefined $ lookup (show req) reqMap
        manager = error "tried to access mock network manager"
-}
