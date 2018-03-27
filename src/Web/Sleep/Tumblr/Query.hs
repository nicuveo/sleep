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
  PFormat(..),
  PRange(..),
  AvatarSize(..),
  HasBlogId(..),
  HasAPIKey(..),
  HasAuthCred(..),
  MayHaveAuthCred(..),
  MonadAuth,
  MonadMaybeAuth,

  -- blog info
  getBlogInfo,
  getInfo,

  -- blog avatar
  getBlogAvatar,
  getAvatar,

  -- blog likes
  getBlogLikes,
  getLikes,

  -- blog likes
  getBlogFollowees,
  getFollowees,

  -- blog likes
  getBlogFollowers,
  getFollowers,

  -- blog post
  getBlogPost,
  getPost,

  -- blog posts
  getBlogPosts,
  getPosts,
  getBlogPostsByType,
  getPostsByType,

  -- blog posts queue
  getBlogQueuedPosts,
  getQueuedPosts,

  -- blog posts draft
  getBlogDraftPosts,
  getDraftPosts,

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
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Network



-- query

data QName = QBlogInfo
           | QBlogAvatar
           | QBlogLikes
           | QBlogFollowees
           | QBlogFollowers
           | QBlogPost
           | QBlogPosts
           | QBlogQueuedPosts
           | QBlogDraftPosts

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

newtype APIKey   = APIKey   AppKey     deriving (Show, Eq, Typeable, IsString)
newtype BlogId   = BlogId   String     deriving (Show, Eq, Typeable, IsString)
newtype Before   = Before   UTCTime    deriving (Show, Eq, Typeable)
newtype BeforeId = BeforeId Int        deriving (Show, Eq, Typeable)
newtype Limit    = Limit    Int        deriving (Show, Eq, Typeable)
newtype Offset   = Offset   Int        deriving (Show, Eq, Typeable)
newtype PFormat  = PFormat  PostFormat deriving (Show, Eq, Typeable)
data PRange      = POffset  Int
                 | PBefore  UTCTime
                 | PAfter   UTCTime
                 deriving (Show, Eq, Typeable)
data AvatarSize  = AS_16 | AS_24 | AS_30 | AS_40 | AS_48 | AS_64 | AS_96 | AS_128 | AS_512
                 deriving (Eq, Typeable)

instance Show AvatarSize where
  show AS_16  =  "16"
  show AS_24  =  "24"
  show AS_30  =  "30"
  show AS_40  =  "40"
  show AS_48  =  "48"
  show AS_64  =  "64"
  show AS_96  =  "96"
  show AS_128 = "128"
  show AS_512 = "512"

instance ToParameter Before     where mkParam (Before    d) = ("before",    B.pack $ show $ toTimestamp d)
instance ToParameter BeforeId   where mkParam (BeforeId  i) = ("before_id", B.pack $ show i)
instance ToParameter Limit      where mkParam (Limit     p) = ("limit",     B.pack $ show $ clamp 1 20 p)
instance ToParameter Offset     where mkParam (Offset    o) = ("offset",    B.pack $ show o)
instance ToParameter PFormat    where mkParam (PFormat   f) = ("filter",    B.pack $ show f)
instance ToParameter Tag        where mkParam (Tag       t) = ("tag",       B.pack t)
instance ToParameter PRange     where mkParam (POffset   o) = ("offset",    B.pack $ show o)
                                      mkParam (PBefore   d) = ("before",    B.pack $ show $ toTimestamp d)
                                      mkParam (PAfter    d) = ("after",     B.pack $ show $ toTimestamp d)


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

instance QueryInfo 'QBlogInfo where
  type QueryResult 'QBlogInfo = Blog
  getMethod = const QGet

getBlogInfo :: MonadMaybeAuth c m => BlogId -> m (Query 'QBlogInfo m)
getBlogInfo (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/info"

getInfo :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QBlogInfo m)
getInfo = asks getBlogId >>= getBlogInfo



-- blog avatar

instance QueryInfo  'QBlogAvatar where
  type QueryResult  'QBlogAvatar = PNGImage
  getMethod = const  QGet

getBlogAvatar :: MonadMaybeAuth c m => BlogId -> AvatarSize -> m (Query 'QBlogAvatar m)
getBlogAvatar (BlogId bid) s = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/avatar/" ++ show s

getAvatar :: (MonadMaybeAuth c m, HasBlogId c) => AvatarSize -> m (Query 'QBlogAvatar m)
getAvatar s = asks getBlogId >>= flip getBlogAvatar s



-- blog likes

instance QueryParam 'QBlogLikes Limit
instance QueryParam 'QBlogLikes PRange
instance QueryInfo  'QBlogLikes where
  type QueryResult  'QBlogLikes = PostList
  getMethod = const  QGet

getBlogLikes :: MonadMaybeAuth c m => BlogId -> m (Query 'QBlogLikes m)
getBlogLikes (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/likes"

getLikes :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QBlogLikes m)
getLikes = asks getBlogId >>= getBlogLikes



-- blog followees

instance QueryParam 'QBlogFollowees Limit
instance QueryParam 'QBlogFollowees Offset
instance QueryInfo  'QBlogFollowees where
  type QueryResult  'QBlogFollowees = BlogSummaryList
  getMethod = const  QGet

getBlogFollowees :: MonadAuth c m => BlogId -> m (Query 'QBlogLikes m)
getBlogFollowees (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/following"

getFollowees :: (MonadAuth c m, HasBlogId c) => m (Query 'QBlogLikes m)
getFollowees = asks getBlogId >>= getBlogFollowees



-- blog followers

instance QueryParam 'QBlogFollowers Limit
instance QueryParam 'QBlogFollowers Offset
instance QueryInfo  'QBlogFollowers where
  type QueryResult  'QBlogFollowers = BlogSummaryList
  getMethod = const  QGet

getBlogFollowers :: MonadAuth c m => BlogId -> m (Query 'QBlogLikes m)
getBlogFollowers (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/followers"

getFollowers :: (MonadAuth c m, HasBlogId c) => m (Query 'QBlogLikes m)
getFollowers = asks getBlogId >>= getBlogFollowers



-- blog post

instance QueryInfo 'QBlogPost where
  type QueryResult 'QBlogPost = PostList
  getMethod = const QGet

getBlogPost :: MonadMaybeAuth c m => BlogId -> PostId -> m (Query 'QBlogPosts m)
getBlogPost (BlogId bid) pid = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts?id=" ++ show pid

getPost :: (MonadMaybeAuth c m, HasBlogId c) => PostId -> m (Query 'QBlogPosts m)
getPost pid = asks getBlogId >>= flip getBlogPost pid



-- blog posts

instance QueryParam 'QBlogPosts Before
instance QueryParam 'QBlogPosts Limit
instance QueryParam 'QBlogPosts Offset
instance QueryParam 'QBlogPosts PFormat
instance QueryParam 'QBlogPosts Tag
instance QueryInfo  'QBlogPosts where
  type QueryResult  'QBlogPosts = PostList
  getMethod = const  QGet

getBlogPosts :: MonadMaybeAuth c m => BlogId -> m (Query 'QBlogPosts m)
getBlogPosts (BlogId bid) = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts"

getPosts :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'QBlogPosts m)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: MonadMaybeAuth c m => BlogId -> PostType -> m (Query 'QBlogPosts m)
getBlogPostsByType (BlogId bid) t = liftMaybeAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/" ++ show t

getPostsByType :: (MonadMaybeAuth c m, HasBlogId c) => PostType -> m (Query 'QBlogPosts m)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog posts queue

instance QueryParam 'QBlogQueuedPosts Limit;
instance QueryParam 'QBlogQueuedPosts Offset;
instance QueryParam 'QBlogQueuedPosts PFormat;
instance QueryInfo  'QBlogQueuedPosts where
  type QueryResult  'QBlogQueuedPosts = PostList
  getMethod = const  QGet

getBlogQueuedPosts :: MonadAuth c m => BlogId -> m (Query 'QBlogQueuedPosts m)
getBlogQueuedPosts (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/queue"

getQueuedPosts :: (MonadAuth c m, HasBlogId c) => m (Query 'QBlogQueuedPosts m)
getQueuedPosts = asks getBlogId >>= getBlogQueuedPosts



-- blog posts draft

instance QueryParam 'QBlogDraftPosts BeforeId;
instance QueryParam 'QBlogDraftPosts PFormat;
instance QueryInfo  'QBlogDraftPosts where
  type QueryResult  'QBlogDraftPosts = PostList
  getMethod = const  QGet

getBlogDraftPosts :: MonadAuth c m => BlogId -> m (Query 'QBlogDraftPosts m)
getBlogDraftPosts (BlogId bid) = liftAddAuth $ mkQuery $ "blog/" ++ bid ++ "/posts/draft"

getDraftPosts :: (MonadAuth c m, HasBlogId c) => m (Query 'QBlogDraftPosts m)
getDraftPosts = asks getBlogId >>= getBlogDraftPosts



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
