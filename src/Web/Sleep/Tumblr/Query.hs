{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}



-- module

module Web.Sleep.Tumblr.Query (
  QMethod(..),
  QName(..),
  Query(..),
  QueryInfo(..),
  Parameter,
  ParametersMap,
  ToParameter(..),
  QueryParam(..),
  (&=),
  toURI,
  toRequest,
  ) where



-- imports

import           Data.ByteString           (ByteString)
import qualified Data.Map.Strict           as M
import           Data.Typeable
import           Network.HTTP.Client
import           Network.HTTP.Types.Method
import           Network.URI



-- exported types

data QMethod = QGet | QPost
data QName = QInfo
           | QLikes
           | QPosts
           | QPostsQueue
           | QPostsDraft

data Query (q :: QName) = Query { request :: Request
                                , params  :: ParametersMap
                                }

type Parameter = (ByteString, ByteString)
type ParametersMap = M.Map TypeRep Parameter

class Typeable p => ToParameter p where
  mkParam :: p -> Parameter

class ToParameter p => QueryParam (q :: QName) p where
  pAdd :: p -> Query q -> Query q
  pAdd p q = q { params = M.insert (typeOf p) (mkParam p) $ params q }

class QueryInfo (q :: QName) where
  type QueryResult q :: *
  getMethod :: Query q -> QMethod



-- exported functions

(&=) :: (Monad m, QueryParam q p) => m (Query q) -> p -> m (Query q)
q &= p = pAdd p <$> q

toURI :: QueryInfo q => Query q -> URI
toURI = getUri . toRequest

toRequest :: QueryInfo q => Query q -> Request
toRequest q = setQueryString queryStr $ reqBase { method = reqMethod
                                                , host   = "api.tumblr.com/v2" }
  where queryStr  = map (fmap Just) $ M.elems $ params q
        reqBase   = request q
        reqMethod = case getMethod q of
                      QGet  -> methodGet
                      QPost -> methodPost



-- instances

instance QueryInfo q => Show (Query q) where
  show = show . toURI
