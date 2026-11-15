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

import Data.ByteString           qualified as BS
import Data.Text.Encoding        (encodeUtf8)
import Network.HTTP.Client       (Request)
import Network.HTTP.Client       qualified as Network
import Network.HTTP.Types.Header qualified as Network
import Web.HttpApiData           (ToHttpApiData)
import Web.HttpApiData           qualified as HttpApiData

addParameter
  :: (ToHttpApiData s)
  => s       -- ^ parameter name
  -> Maybe s -- ^ parameter value
  -> Request
  -> Request
addParameter name value req =
  req { Network.queryString = append $ Network.queryString req }
  where
    parameter =
      encodeUtf8 $ HttpApiData.toQueryParam name <>
        case value of
          Nothing  -> ""
          Just val -> "=" <> HttpApiData.toQueryParam val
    append qs
      | BS.null qs = BS.concat [         parameter]
      | otherwise  = BS.concat [qs, "&", parameter]

addParameters
  :: (ToHttpApiData s, Foldable t)
  => t (s, Maybe s)
  -> Request
  -> Request
addParameters =
  flip $ foldl' $ flip $ uncurry addParameter

addHeader
  :: (ToHttpApiData s)
  => Network.HeaderName
  -> s
  -> Request
  -> Request
addHeader hn hv req =
  req { Network.requestHeaders =
        Network.requestHeaders req ++ [(hn, HttpApiData.toHeader hv)]
      }

setBody
  :: BS.ByteString
  -> Request
  -> Request
setBody body req =
  req { Network.requestBody = Network.RequestBodyBS body }
