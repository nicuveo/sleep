{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Query (
  Query(..),
  Parameter,
  ParametersMap,
  ToParameter(..),
  QueryParam(..),
  (&=),
  ) where



-- imports

import           Control.Monad
import qualified Data.Map      as M
import           Data.Typeable
import           Network.URL



-- exported types

data Query q r = Query { path   :: String
                       , params :: ParametersMap
                       }

type Parameter = (String, String)
type ParametersMap = M.Map TypeRep Parameter

class ToParameter p where
  mkParam :: p -> Parameter

class (Typeable p, ToParameter p) => QueryParam q p where
  pAdd :: Query q r -> p -> Query q r
  pAdd q p = q { params = M.insert (typeOf p) (mkParam p) $ params q }



-- exported functions

(&=) :: (Monad m, QueryParam q p) => m (Query q r) -> p -> m (Query q r)
q &= p = liftM2 pAdd q $ pure p



-- instances

instance Show (Query q r) where
  show (Query path params) = exportURL $ URL urlType path $ M.elems params
    where urlType = Absolute $ Host (HTTP True) "api.tumblr.com/v2" Nothing
