{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}



-- module

module Web.Sleep.Tumblr.Query (

  -- query
  QMethod(..),
  QName(..),
  Query,
  QueryInfo(..),
  Parameter,
  ParametersMap,
  ToParameter(..),
  QueryParam(..),

  -- query functions
  (&=),
  toUnsignedRequest,
  toUnsignedURI,
  toURI,

  -- parameters
  BlogId(..),
  APIKey(..),
  Limit(..),
  Offset(..),
  PType(..),
  HasBlogId(..),
  HasAPIKey(..),
  HasAuthCred(..),
  MayHaveAuthCred(..),
  MonadAuth,
  MonadMaybeAuth,

  -- blog info
  getBlogInfo,
  getInfo,

  -- blog likes
  getBlogLikes,
  getLikes,

  -- blog posts
  getBlogPosts,
  getPosts,
  getBlogPostsByType,
  getPostsByType,

  -- blog posts queue
  getBlogPostsQueue,
  getPostsQueue,

  -- blog posts draft
  getBlogPostsDraft,
  getPostsDraft,

  ) where



-- imports

import           Control.Monad.Reader
import qualified Data.ByteString           as B (ByteString)
import qualified Data.ByteString.Char8     as B
import qualified Data.Map.Strict           as M
import           Data.String
import           Data.Time.Clock
import           Data.Typeable
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.URI               as N (URI)

import           Web.Sleep.Common.Misc
import           Web.Sleep.Common.Network
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Data



-- query

data QName = QInfo
           | QLikes
           | QPosts
           | QPostsQueue
           | QPostsDraft

data Query (q :: QName) m = Query { request :: N.Request
                                  , params  :: ParametersMap
                                  , sign    :: N.Request -> m N.Request
                                  }

type Parameter = (B.ByteString, B.ByteString)
type ParametersMap = M.Map TypeRep Parameter

class Typeable p => ToParameter p where
  mkParam :: p -> Parameter

class ToParameter p => QueryParam (q :: QName) p where
  pAdd :: p -> Query q m -> Query q m
  pAdd p q = q { params = M.insert (typeOf p) (mkParam p) $ params q }

class QueryInfo (q :: QName) where
  type QueryResult q :: *
  getMethod :: Query q m -> QMethod



-- query instances

instance Monad m => ToRequest (Query q m) m where
  type RequestResult (Query q m) = QueryResult q
  toRequest q = sign q $ toUnsignedRequest q

instance QueryInfo q => Show (Query q m) where
  show = show . toUnsignedURI



-- query functions

(&=) :: (Functor f, QueryParam q p) => f (Query q m) -> p -> f (Query q m)
q &= p = pAdd p <$> q

toUnsignedRequest :: Query q m -> N.Request
toUnsignedRequest q = N.setQueryString queryStr $ request q
  where queryStr = fmap (fmap Just) $ M.elems $ params q

toUnsignedURI :: Query q m -> N.URI
toUnsignedURI = N.getUri . toUnsignedRequest

toURI :: Monad m => Query q m -> m N.URI
toURI = fmap N.getUri . toRequest



-- parameters

newtype BlogId = BlogId  String   deriving (Show, Eq, Typeable, IsString)
newtype APIKey = APIKey  AppKey   deriving (Show, Eq, Typeable, IsString)
newtype Limit  = Limit   Int      deriving (Show, Eq, Typeable)
newtype Offset = Offset  Int      deriving (Show, Eq, Typeable)
newtype PType  = PType   PostType deriving (Show, Eq, Typeable)
data PostRange = POffset Int
               | PBefore UTCTime
               | PAfter  UTCTime
               deriving (Show, Eq, Typeable)

instance ToParameter Limit     where mkParam (Limit     p) = ("limit",  B.pack $ show $ clamp 1 20 p)
instance ToParameter Offset    where mkParam (Offset    o) = ("offset", B.pack $ show o)
instance ToParameter PType     where mkParam (PType     p) = ("type",   B.pack $ show p)
instance ToParameter PostRange where mkParam (POffset   o) = ("offset", B.pack $ show o)
                                     mkParam (PBefore   d) = ("before", B.pack $ show $ toTimestamp d)
                                     mkParam (PAfter    d) = ("after",  B.pack $ show $ toTimestamp d)

class HasBlogId a where
  getBlogId :: a -> BlogId

class HasAPIKey a where
  getAPIKey :: a -> APIKey

class HasAuthCred a where
  getAuthCred :: a -> AuthCred

class MayHaveAuthCred a where
  maybeGetAuthCred :: a -> Maybe AuthCred
  default maybeGetAuthCred :: HasAuthCred a => a -> Maybe AuthCred
  maybeGetAuthCred = Just . getAuthCred

type MonadAuth      c m = (MonadReader c m, MonadSign m, HasAuthCred c)
type MonadMaybeAuth c m = (MonadReader c m, MonadSign m, HasAPIKey c, MayHaveAuthCred c)



-- parameter instances

instance {-# OVERLAPPABLE #-} MayHaveAuthCred a where
  maybeGetAuthCred = const Nothing

instance HasBlogId BlogId where { getBlogId = id }
instance HasAPIKey APIKey where { getAPIKey = id }



-- blog info

instance QueryInfo  'QInfo where
  type QueryResult  'QInfo = Blog
  getMethod = const QGet

getBlogInfo :: MonadMaybeAuth c m => BlogId -> m (Query 'QInfo m)
getBlogInfo (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QInfo m)
getInfo = asks getBlogId >>= getBlogInfo



-- blog likes

instance QueryParam 'QLikes Limit
instance QueryParam 'QLikes PostRange
instance QueryInfo  'QLikes where
  type QueryResult  'QLikes = PostList
  getMethod = const QGet

getBlogLikes :: MonadMaybeAuth c m => BlogId -> m (Query 'QLikes m)
getBlogLikes (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QLikes m)
getLikes = asks getBlogId >>= getBlogLikes



-- blog posts

instance QueryParam 'QPosts Limit;
instance QueryParam 'QPosts Offset;
instance QueryParam 'QPosts PType;
instance QueryInfo  'QPosts where
  type QueryResult  'QPosts = PostList
  getMethod = const QGet

getBlogPosts :: MonadMaybeAuth c m => BlogId -> m (Query 'QPosts m)
getBlogPosts (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QPosts m)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: MonadMaybeAuth c m => BlogId -> PostType -> m (Query 'QPosts m)
getBlogPostsByType (BlogId bid) t = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadMaybeAuth c m, HasBlogId c) => PostType -> m (Query 'QPosts m)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

instance QueryParam 'QPostsQueue Limit;
instance QueryInfo  'QPostsQueue where
  type QueryResult  'QPostsQueue = PostList
  getMethod = const QGet

getBlogPostsQueue :: MonadAuth c m => BlogId -> m (Query 'QPostsQueue m)
getBlogPostsQueue (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getPostsQueue :: (MonadAuth c m, HasBlogId c) => m (Query 'QPostsQueue m)
getPostsQueue = asks getBlogId >>= getBlogPostsQueue



-- blog posts draft

instance QueryInfo  'QPostsDraft where
  type QueryResult  'QPostsDraft = PostList
  getMethod = const QGet

getBlogPostsDraft :: MonadAuth c m => BlogId -> m (Query 'QPostsDraft m)
getBlogPostsDraft (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getPostsDraft :: (MonadAuth c m, HasBlogId c) => m (Query 'QPostsDraft m)
getPostsDraft = asks getBlogId >>= getBlogPostsDraft



-- local helpers

mkQuery :: (Monad m, QueryInfo q) => String -> m (Query q m)
mkQuery s = return resultQuery
  where resultQuery = Query req M.empty return
        req = N.defaultRequest { N.path   = fromString $ "/v2/" ++ s
                               , N.method = reqMethod
                               , N.host   = "api.tumblr.com"
                               }
        reqMethod = case getMethod resultQuery of
                      QGet  -> N.methodGet
                      QPost -> N.methodPost

liftMaybeAddAuth :: MonadMaybeAuth c m => m (Query q m) -> m (Query q m)
liftMaybeAddAuth query = do
  q <- query
  APIKey k <- asks getAPIKey
  ma <- asks maybeGetAuthCred
  case ma of
    Nothing -> return $ q { sign = addAPIKey k }
    Just a  -> return $ q { sign = signOAuth a }
  where addAPIKey k = return . appendParam ("api_key", k)

liftAddAuth :: MonadAuth c m => m (Query q m) -> m (Query q m)
liftAddAuth query = do
  q <- query
  a <- asks getAuthCred
  return $ q { sign = signOAuth a }
