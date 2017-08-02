{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Tumblr.Query (
  QProtocol(..),
  QName(..),
  Query(..),
  QueryProtocol,
  QueryResult,
  Parameter,
  ParametersMap,
  ToParameter(..),
  QueryParam(..),
  (&=),
  toURL,
  ) where



-- imports

import           Control.Monad
import qualified Data.Map      as M
import           Data.Typeable
import           Network.URL



-- exported types

data QProtocol = QGet | QPost
data QName = QInfo
           | QLikes
           | QPosts
           | QPostsQueue
           | QPostsDraft

data Query (q :: QName) = Query { path   :: String
                                , params :: ParametersMap
                                }

type family QueryProtocol (q :: QName) :: QProtocol
type family QueryResult   (q :: QName) :: *

type Parameter = (String, String)
type ParametersMap = M.Map TypeRep Parameter

class Typeable p => ToParameter p where
  mkParam :: p -> Parameter

class ToParameter p => QueryParam (q :: QName) p where
  pAdd :: Query q -> p -> Query q
  pAdd q p = q { params = M.insert (typeOf p) (mkParam p) $ params q }



-- exported functions

(&=) :: (Monad m, QueryParam q p) => m (Query q) -> p -> m (Query q)
q &= p = liftM2 pAdd q $ pure p

toURL :: Query q -> URL
toURL (Query path params) = URL urlType path $ M.elems params
  where urlType = Absolute $ Host (HTTP True) "api.tumblr.com/v2" Nothing



-- instances

instance Show (Query q) where
  show = exportURL . toURL
