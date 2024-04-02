{- |
Collection of helpers around the 'Request' type from @http-client@.
-}

module Web.Sleep.Core.Request
  ( addParameter
  , addParameters
  , addHeader
  , setBody
  ) where

import "this" Prelude

import Data.ByteString           qualified as B
import Network.HTTP.Client       qualified as N
import Network.HTTP.Types.Header qualified as N


addParameter :: B.ByteString -> B.ByteString -> N.Request -> N.Request
addParameter name val req = req { N.queryString = append $ N.queryString req }
  where
    append qs
      | B.null qs = B.concat [         name, "=", val]
      | otherwise = B.concat [qs, "&", name, "=", val]

addParameters :: Foldable t => t (B.ByteString, B.ByteString) -> N.Request -> N.Request
addParameters = flip $ foldl' $ flip $ uncurry addParameter

addHeader :: N.HeaderName -> B.ByteString -> N.Request -> N.Request
addHeader hn hv req = req { N.requestHeaders = N.requestHeaders req ++ [ (hn, hv) ] }

setBody :: B.ByteString -> N.Request -> N.Request
setBody body req = req { N.requestBody = N.RequestBodyBS body }
