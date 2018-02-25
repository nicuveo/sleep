{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}



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

data Query (q :: QName) = Query { request :: N.Request
                                , params  :: ParametersMap
                                , post    :: N.Request -> N.Request
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


-- query instances

instance QueryInfo q => ToRequest (Query q) where
  toRequest q = post q $ N.setQueryString queryStr $ reqBase
                { N.method = reqMethod
                , N.host   = "api.tumblr.com/v2"
                }
    where queryStr  = fmap (fmap Just) $ M.elems $ params q
          reqBase   = request q
          reqMethod = case getMethod q of
                        QGet  -> N.methodGet
                        QPost -> N.methodPost

instance QueryInfo q => Show (Query q) where
  show = show . toURI



-- query functions

(&=) :: (Functor f, QueryParam q p) => f (Query q) -> p -> f (Query q)
q &= p = pAdd p <$> q

toURI :: QueryInfo q => Query q -> N.URI
toURI = N.getUri . toRequest



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

class HasAPIKey a => HasAuthCred a where
  addAuth :: a -> N.Request -> N.Request

class HasAPIKey a => MayHaveAuthCred a where
  maybeAddAuth :: a -> N.Request -> N.Request



-- parameter instances

instance {-# OVERLAPPABLE #-} HasAPIKey a => MayHaveAuthCred a where
  maybeAddAuth = const id

instance HasBlogId BlogId where { getBlogId = id }
instance HasAPIKey APIKey where { getAPIKey = id }



-- blog info

instance QueryParam 'QInfo APIKey
instance QueryInfo  'QInfo where
  type QueryResult  'QInfo = Blog
  getMethod = const QGet

getBlogInfo :: (MonadReader c m, MayHaveAuthCred c) => BlogId -> m (Query 'QInfo)
getBlogInfo (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadReader c m, MayHaveAuthCred c, HasBlogId c) => m (Query 'QInfo)
getInfo = asks getBlogId >>= getBlogInfo



-- blog likes

instance QueryParam 'QLikes APIKey
instance QueryParam 'QLikes Limit
instance QueryParam 'QLikes PostRange
instance QueryInfo  'QLikes where
  type QueryResult  'QLikes = PostList
  getMethod = const QGet

getBlogLikes :: (MonadReader c m, MayHaveAuthCred c) => BlogId -> m (Query 'QLikes)
getBlogLikes (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadReader c m, MayHaveAuthCred c, HasBlogId c) => m (Query 'QLikes)
getLikes = asks getBlogId >>= getBlogLikes



-- blog posts

instance QueryParam 'QPosts APIKey;
instance QueryParam 'QPosts Limit;
instance QueryInfo  'QPosts where
  type QueryResult  'QPosts = PostList
  getMethod = const QGet

getBlogPosts :: (MonadReader c m, MayHaveAuthCred c) => BlogId -> m (Query 'QPosts)
getBlogPosts (BlogId bid) = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadReader c m, MayHaveAuthCred c, HasBlogId c) => m (Query 'QPosts)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: (MonadReader c m, MayHaveAuthCred c) => BlogId -> PostType -> m (Query 'QPosts)
getBlogPostsByType (BlogId bid) t = liftMaybeAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadReader c m, MayHaveAuthCred c, HasBlogId c) => PostType -> m (Query 'QPosts)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

instance QueryParam 'QPostsQueue APIKey;
instance QueryParam 'QPostsQueue Limit;
instance QueryInfo  'QPostsQueue where
  type QueryResult  'QPostsQueue = PostList
  getMethod = const QGet

getBlogPostsQueue :: (MonadReader c m, HasAuthCred c) => BlogId -> m (Query 'QPostsQueue)
getBlogPostsQueue (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getPostsQueue :: (MonadReader c m, HasAuthCred c, HasBlogId c) => m (Query 'QPostsQueue)
getPostsQueue = asks getBlogId >>= getBlogPostsQueue



-- blog posts draft

instance QueryParam 'QPostsDraft APIKey;
instance QueryInfo  'QPostsDraft where
  type QueryResult  'QPostsDraft = PostList
  getMethod = const QGet

getBlogPostsDraft :: (MonadReader c m, HasAuthCred c) => BlogId -> m (Query 'QPostsDraft)
getBlogPostsDraft (BlogId bid) = liftAddAuth $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getPostsDraft :: (MonadReader c m, HasAuthCred c, HasBlogId c) => m (Query 'QPostsDraft)
getPostsDraft = asks getBlogId >>= getBlogPostsDraft



-- local helpers

mkQuery :: Monad m => String -> m (Query q)
mkQuery = return . fromUrl . N.parseRequest_
  where fromUrl u = Query u M.empty id

addAPIKey :: (MonadReader c m, HasAPIKey c, QueryParam q APIKey) => m (Query q) -> m (Query q)
addAPIKey q = asks getAPIKey >>= (q &=)

liftMaybeAddAuth :: (MonadReader c m, MayHaveAuthCred c) => m (Query q) -> m (Query q)
liftMaybeAddAuth query = do
  q <- query
  c <- ask
  return $ q { post = maybeAddAuth c . post q }

liftAddAuth :: (MonadReader c m, HasAuthCred c) => m (Query q) -> m (Query q)
liftAddAuth query = do
  q <- query
  c <- ask
  return $ q { post = addAuth c . post q }
