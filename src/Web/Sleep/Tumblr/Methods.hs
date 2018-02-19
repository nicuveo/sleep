{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}



-- module

module Web.Sleep.Tumblr.Methods (
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
import           Data.ByteString.Char8
import qualified Data.Map               as M
import           Data.String
import           Data.Time.Clock
import           Data.Typeable

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Query



-- parameters

newtype BlogId    = BlogId    String deriving (Show, Eq, Typeable, IsString)
newtype APIKey    = APIKey    AppKey deriving (Show, Eq, Typeable, IsString)
newtype Limit     = Limit     Int    deriving (Show, Eq, Typeable)
newtype Offset    = Offset    Int    deriving (Show, Eq, Typeable)
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
  tryGetAuthCred :: a -> Maybe AuthCred

instance {-# OVERLAPPABLE #-} MayHaveAuthCred a where
  tryGetAuthCred _ = Nothing



-- obvious instances

instance HasBlogId BlogId where { getBlogId = id }
instance HasAPIKey APIKey where { getAPIKey = id }



-- blog info

instance QueryParam 'QInfo APIKey
instance QueryInfo  QInfo where
  type QueryResult  QInfo = Blog
  getMethod = const QGet

getBlogInfo :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c) => BlogId -> m (Query QInfo)
getBlogInfo (BlogId bid) = addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c, HasBlogId c) => m (Query QInfo)
getInfo = asks getBlogId >>= getBlogInfo



-- blog likes

instance QueryParam QLikes APIKey
instance QueryParam QLikes Limit
instance QueryParam QLikes PostRange
instance QueryInfo  QLikes where
  type QueryResult  QLikes = PostList
  getMethod = const QGet

getBlogLikes :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c) => BlogId -> m (Query QLikes)
getBlogLikes (BlogId bid) = addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c, HasBlogId c) => m (Query QLikes)
getLikes = asks getBlogId >>= getBlogLikes



-- blog posts

instance QueryParam QPosts APIKey;
instance QueryParam QPosts Limit;
instance QueryInfo  QPosts where
  type QueryResult  QPosts = PostList
  getMethod = const QGet

getBlogPosts :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c) => BlogId -> m (Query QPosts)
getBlogPosts (BlogId bid) = addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c, HasBlogId c) => m (Query QPosts)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c) => BlogId -> PostType -> m (Query QPosts)
getBlogPostsByType (BlogId bid) t = addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadReader c m, HasAPIKey c, MayHaveAuthCred c, HasBlogId c) => PostType -> m (Query QPosts)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

instance QueryParam QPostsQueue APIKey;
instance QueryParam QPostsQueue Limit;
instance QueryInfo  QPostsQueue where
  type QueryResult  QPostsQueue = PostList
  getMethod = const QGet

getBlogPostsQueue :: (MonadReader c m, HasAuthCred c) => BlogId -> m (Query QPostsQueue)
getBlogPostsQueue (BlogId bid) = mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getPostsQueue :: (MonadReader c m, HasAuthCred c, HasBlogId c) => m (Query QPostsQueue)
getPostsQueue = asks getBlogId >>= getBlogPostsQueue



-- blog posts draft

instance QueryParam QPostsDraft APIKey;
instance QueryInfo  QPostsDraft where
  type QueryResult  QPostsDraft = PostList
  getMethod = const QGet

getBlogPostsDraft :: (MonadReader c m, HasAuthCred c) => BlogId -> m (Query QPostsDraft)
getBlogPostsDraft (BlogId bid) = mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getPostsDraft :: (MonadReader c m, HasAPIKey c, HasAuthCred c, HasBlogId c) => m (Query QPostsDraft)
getPostsDraft = asks getBlogId >>= getBlogPostsDraft



-- local helpers

mkQuery :: Monad m => String -> m (Query q)
mkQuery = return . flip Query M.empty . parseUrl
  where parseUrl = undefined

addAPIKey :: (MonadReader c m, HasAPIKey c, QueryParam q APIKey) => m (Query q) -> m (Query q)
addAPIKey q = asks getAPIKey >>= (q &=)
