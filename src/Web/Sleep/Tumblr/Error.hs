-- module

module Web.Sleep.Tumblr.Error (Error(..)) where



-- imports

import           Text.Printf



-- exported types

data Error = ServerError { errorCode :: Int, errorMsg :: String }
           | ClientError { errorCode :: Int, errorMsg :: String }



-- instances

instance Show Error where
  show (ServerError c m) = printf "server error: %s (code %d)" m c
  show (ClientError c m) = printf "client error: %s (code %d)" m c
