{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}



-- module

module Web.Sleep.Tumblr.Methods (
  -- parameters
  BlogId(..),
  APIKey(..),
  AuthToken(..),
  Limit(..),
  HasBlogId(..),
  HasAPIKey(..),
  HasAuthToken(..),
  MayHaveAuthToken(..),
  -- blog info
  QInfo,
  getBlogInfo,
  getInfo,
  -- blog info
  QLikes,
  getBlogLikes,
  getLikes,
  -- blog posts
  QPosts,
  getBlogPosts,
  getPosts,
  getBlogPostsByType,
  getPostsByType,
  -- blog posts queue
  QPostsQueue,
  getBlogPostsQueue,
  getPostsQueue,
  -- blog posts draft
  QPostsDraft,
  getBlogPostsDraft,
  getPostsDraft,
  ) where



-- imports

import           Control.Monad.Reader
import qualified Data.Map               as M
import           Data.Time.Clock
import           Data.Typeable

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Query



-- parameters

newtype BlogId    = BlogId    String deriving (Show, Eq, Typeable)
newtype APIKey    = APIKey    String deriving (Show, Eq, Typeable)
newtype AuthToken = AuthToken String deriving (Show, Eq, Typeable)
newtype Limit     = Limit     Int    deriving (Show, Eq, Typeable)
data PostRange = Offset Int
               | Before UTCTime
               | After  UTCTime
               deriving (Show, Eq, Typeable)

instance ToParameter APIKey    where mkParam (APIKey    p) = ("api-key", p)
instance ToParameter AuthToken where mkParam (AuthToken p) = ("auth-token", p)
instance ToParameter Limit     where mkParam (Limit     p) = ("limit", show $ clamp 1 20 p)
instance ToParameter PostRange where mkParam (Offset    o) = ("offset", show o)
                                     mkParam (Before    d) = ("before", show $ toTimestamp d)
                                     mkParam (After     d) = ("after",  show $ toTimestamp d)

class HasBlogId a where
  getBlogId :: a -> BlogId

class HasAPIKey a where
  getAPIKey :: a -> APIKey

class HasAuthToken a where
  getAuthToken :: a -> AuthToken

class MayHaveAuthToken a where
  tryGetAuthToken :: a -> Maybe AuthToken

instance {-# OVERLAPPABLE #-} MayHaveAuthToken a where
  tryGetAuthToken _ = Nothing



-- blog info

data QInfo;
instance QueryParam QInfo APIKey;
instance QueryParam QInfo AuthToken;

getBlogInfo :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c) => BlogId -> m (Query QInfo Blog)
getBlogInfo (BlogId bid) = tryAddAuthToken $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c, HasBlogId c) => m (Query QInfo Blog)
getInfo = asks getBlogId >>= getBlogInfo



-- blog likes

data QLikes;
instance QueryParam QLikes APIKey;
instance QueryParam QLikes AuthToken;
instance QueryParam QLikes Limit;
instance QueryParam QLikes PostRange;

getBlogLikes :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c) => BlogId -> m (Query QLikes PostList)
getBlogLikes (BlogId bid) = tryAddAuthToken $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c, HasBlogId c) => m (Query QLikes PostList)
getLikes = asks getBlogId >>= getBlogLikes



-- blog posts

data QPosts;
instance QueryParam QPosts APIKey;
instance QueryParam QPosts AuthToken;
instance QueryParam QPosts Limit;

getBlogPosts :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c) => BlogId -> m (Query QPosts PostList)
getBlogPosts (BlogId bid) = tryAddAuthToken $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c, HasBlogId c) => m (Query QPosts PostList)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c) => BlogId -> PostType -> m (Query QPosts PostList)
getBlogPostsByType (BlogId bid) t = tryAddAuthToken $ addAPIKey $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadReader c m, HasAPIKey c, MayHaveAuthToken c, HasBlogId c) => PostType -> m (Query QPosts PostList)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

data QPostsQueue;
instance QueryParam QPostsQueue APIKey;
instance QueryParam QPostsQueue AuthToken;
instance QueryParam QPostsQueue Limit;

getBlogPostsQueue :: (MonadReader c m, HasAuthToken c) => BlogId -> m (Query QPostsQueue PostList)
getBlogPostsQueue (BlogId bid) = addAuthToken $ mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getPostsQueue :: (MonadReader c m, HasAPIKey c, HasAuthToken c, HasBlogId c) => m (Query QPostsQueue PostList)
getPostsQueue = asks getBlogId >>= getBlogPostsQueue



-- blog posts draft

data QPostsDraft;
instance QueryParam QPostsDraft APIKey;
instance QueryParam QPostsDraft AuthToken;

getBlogPostsDraft :: (MonadReader c m, HasAuthToken c) => BlogId -> m (Query QPostsDraft PostList)
getBlogPostsDraft (BlogId bid) = addAuthToken $ mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getPostsDraft :: (MonadReader c m, HasAPIKey c, HasAuthToken c, HasBlogId c) => m (Query QPostsDraft PostList)
getPostsDraft = asks getBlogId >>= getBlogPostsDraft



-- local helpers

mkQuery :: Monad m => String -> m (Query q r)
mkQuery = return . flip Query M.empty

addAPIKey :: (MonadReader c m, HasAPIKey c, QueryParam q APIKey) => m (Query q r) -> m (Query q r)
addAPIKey q = asks getAPIKey >>= (q &=)

addAuthToken :: (MonadReader c m, HasAuthToken c, QueryParam q AuthToken) => m (Query q r) -> m (Query q r)
addAuthToken q = asks getAuthToken >>= (q &=)

tryAddAuthToken :: (MonadReader c m, MayHaveAuthToken c, QueryParam q AuthToken) => m (Query q r) -> m (Query q r)
tryAddAuthToken q = asks tryGetAuthToken >>= doIt
  where doIt Nothing  = q
        doIt (Just t) = q &= t
