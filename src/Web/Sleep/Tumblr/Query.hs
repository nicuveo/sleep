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
  HasBlogId(..),
  HasAPIKey(..),
  HasAuthCred(..),
  MayHaveAuthCred(..),

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
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8
import qualified Data.Map.Strict           as M
import           Data.String
import           Data.Time.Clock
import           Data.Typeable
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.URI               as N

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

type Parameter = (ByteString, ByteString)
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

instance (QueryInfo q, Monad m) => ToRequest (Query q m) m where
  type RequestResult (Query q m) = QueryResult q
  toRequest q = sign q $ toUnsignedRequest q

instance QueryInfo q => Show (Query q m) where
  show = show . toUnsignedURI



-- query functions

(&=) :: (Functor f, QueryParam q p) => f (Query q m) -> p -> f (Query q m)
q &= p = pAdd p <$> q

toUnsignedRequest :: QueryInfo q => Query q m -> N.Request
toUnsignedRequest q = N.setQueryString queryStr $ reqBase
                      { N.method = reqMethod
                      , N.host   = "api.tumblr.com/v2"
                      }
  where queryStr  = fmap (fmap Just) $ M.elems $ params q
        reqBase   = request q
        reqMethod = case getMethod q of
                      QGet  -> N.methodGet
                      QPost -> N.methodPost

toUnsignedURI :: QueryInfo q => Query q m -> N.URI
toUnsignedURI = N.getUri . toUnsignedRequest

toURI :: (Monad m, QueryInfo q) => Query q m -> m N.URI
toURI = fmap N.getUri . toRequest



-- parameters

newtype BlogId = BlogId    String deriving (Show, Eq, Typeable, IsString)
newtype APIKey = APIKey    AppKey deriving (Show, Eq, Typeable, IsString)
newtype Limit  = Limit     Int    deriving (Show, Eq, Typeable)
newtype Offset = Offset    Int    deriving (Show, Eq, Typeable)
data PostRange = POffset Int
               | PBefore UTCTime
               | PAfter  UTCTime
               deriving (Show, Eq, Typeable)

instance ToParameter APIKey    where mkParam (APIKey    p) = ("api_key", p)
instance ToParameter Limit     where mkParam (Limit     p) = ("limit",  pack $ show $ clamp 1 20 p)
instance ToParameter Offset    where mkParam (Offset    o) = ("offset", pack $ show o)
instance ToParameter PostRange where mkParam (POffset   o) = ("offset", pack $ show o)
                                     mkParam (PBefore   d) = ("before", pack $ show $ toTimestamp d)
                                     mkParam (PAfter    d) = ("after",  pack $ show $ toTimestamp d)

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

type MonadAuth c m = (MonadReader c m, MonadSign m, HasAPIKey c, HasAuthCred c)
type MonadMaybeAuth c m = (MonadReader c m, MonadSign m, HasAPIKey c, MayHaveAuthCred c)



-- parameter instances

instance {-# OVERLAPPABLE #-} MayHaveAuthCred a where
  maybeGetAuthCred = const Nothing

instance HasBlogId BlogId where { getBlogId = id }
instance HasAPIKey APIKey where { getAPIKey = id }



-- blog info

instance QueryParam 'QInfo APIKey
instance QueryInfo  'QInfo where
  type QueryResult  'QInfo = Blog
  getMethod = const QGet

getBlogInfo :: MonadMaybeAuth c m => BlogId -> m (Query 'QInfo m)
getBlogInfo (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QInfo m)
getInfo = asks getBlogId >>= getBlogInfo



-- blog likes

instance QueryParam 'QLikes APIKey
instance QueryParam 'QLikes Limit
instance QueryParam 'QLikes PostRange
instance QueryInfo  'QLikes where
  type QueryResult  'QLikes = PostList
  getMethod = const QGet

getBlogLikes :: MonadMaybeAuth c m => BlogId -> m (Query 'QLikes m)
getBlogLikes (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QLikes m)
getLikes = asks getBlogId >>= getBlogLikes



-- blog posts

instance QueryParam 'QPosts APIKey;
instance QueryParam 'QPosts Limit;
instance QueryInfo  'QPosts where
  type QueryResult  'QPosts = PostList
  getMethod = const QGet

getBlogPosts :: MonadMaybeAuth c m => BlogId -> m (Query 'QPosts m)
getBlogPosts (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QPosts m)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: MonadMaybeAuth c m => BlogId -> PostType -> m (Query 'QPosts m)
getBlogPostsByType (BlogId bid) t = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadMaybeAuth c m, HasBlogId c) => PostType -> m (Query 'QPosts m)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

instance QueryParam 'QPostsQueue APIKey;
instance QueryParam 'QPostsQueue Limit;
instance QueryInfo  'QPostsQueue where
  type QueryResult  'QPostsQueue = PostList
  getMethod = const QGet

getBlogPostsQueue :: MonadAuth c m => BlogId -> m (Query 'QPostsQueue m)
getBlogPostsQueue (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getPostsQueue :: (MonadAuth c m, HasBlogId c) => m (Query 'QPostsQueue m)
getPostsQueue = asks getBlogId >>= getBlogPostsQueue



-- blog posts draft

instance QueryParam 'QPostsDraft APIKey;
instance QueryInfo  'QPostsDraft where
  type QueryResult  'QPostsDraft = PostList
  getMethod = const QGet

getBlogPostsDraft :: MonadAuth c m => BlogId -> m (Query 'QPostsDraft m)
getBlogPostsDraft (BlogId bid) = liftAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getPostsDraft :: (MonadAuth c m, HasBlogId c) => m (Query 'QPostsDraft m)
getPostsDraft = asks getBlogId >>= getBlogPostsDraft



-- local helpers

mkQuery :: Monad m => String -> m (Query q m)
mkQuery = return . fromUrl . N.parseRequest_
  where fromUrl u = Query u M.empty return

addAPIKey :: (MonadReader c m, HasAPIKey c, QueryParam q APIKey) => m (Query q m) -> m (Query q m)
addAPIKey q = asks getAPIKey >>= (q &=)

liftMaybeAddAuth :: MonadMaybeAuth c m => m (Query q m) -> m (Query q m)
liftMaybeAddAuth query = do
  q <- query
  ma <- asks maybeGetAuthCred
  case ma of
    Nothing -> return q
    Just a  -> return $ q { sign = signOAuth a }

liftAddAuth :: MonadAuth c m => m (Query q m) -> m (Query q m)
liftAddAuth query = do
  q <- query
  a <- asks getAuthCred
  return $ q { sign = signOAuth a }
