-- module

module Web.Sleep.Libs.Request (
  appendParam,
  appendParams,
  appendHeader,
  setBody,
  ) where



-- imports

import qualified Data.ByteString           as B
import           Data.List                 (foldl')
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Header as N



-- request helpers

appendParam :: (B.ByteString, B.ByteString) -> N.Request -> N.Request
appendParam p req = req { N.queryString = append p $ N.queryString req }
  where append (name, val) "" = B.concat [         name, "=", val]
        append (name, val) qs = B.concat [qs, "&", name, "=", val]

appendParams :: [(B.ByteString, B.ByteString)] -> N.Request -> N.Request
appendParams = flip $ foldl' $ flip appendParam

appendHeader :: N.HeaderName -> B.ByteString -> N.Request -> N.Request
appendHeader hn hv req = req { N.requestHeaders = N.requestHeaders req ++ [ (hn, hv) ] }

setBody :: B.ByteString -> N.Request -> N.Request
setBody body req = req { N.requestBody = N.RequestBodyBS body }
