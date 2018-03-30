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
  QueryName(..),
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
  Before(..),
  BeforeId(..),
  Filter(..),
  Format(..),
  Limit(..),
  Offset(..),
  Title(..),
  Tag(..),
  State(..),
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

  -- post new text
  postNewBlogText,
  postNewText,

  ) where



-- imports

import           Control.Monad.Reader
import qualified Data.ByteString           as B (ByteString)
import qualified Data.Map.Strict           as M
import           Data.String
import           Data.Time.Clock
import           Data.Typeable
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.URI               as N (URI)
import qualified Network.URI.Encode        as N

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Network



-- query

data QueryName = GetBlogInfo
               | GetBlogAvatar
               | GetBlogLikes
               | GetBlogFollowees
               | GetBlogFollowers
               | GetBlogPost
               | GetBlogPosts
               | GetBlogQueuedPosts
               | GetBlogDraftPosts
               | PostText

data Query (q :: QueryName) m = Query { request :: N.Request
                                      , params  :: ParametersMap
                                      , sign    :: N.Request -> m N.Request
                                      }

type Parameter = (B.ByteString, B.ByteString)
type ParametersMap = M.Map TypeRep Parameter

class Typeable p => ToParameter p where
  mkParam :: p -> Parameter

class ToParameter p => QueryParam (q :: QueryName) p where
  pAdd :: p -> Query q m -> Query q m
  pAdd p q = q { params = M.insert (typeOf p) (mkParam p) $ params q }

class QueryInfo (q :: QueryName) where
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
newtype Title    = Title    String     deriving (Show, Eq, Typeable, IsString)
newtype Before   = Before   UTCTime    deriving (Show, Eq, Typeable)
newtype BeforeId = BeforeId Int        deriving (Show, Eq, Typeable)
newtype Filter   = Filter   PostFormat deriving (Show, Eq, Typeable)
newtype Format   = Format   PostFormat deriving (Show, Eq, Typeable)
newtype Limit    = Limit    Int        deriving (Show, Eq, Typeable)
newtype Offset   = Offset   Int        deriving (Show, Eq, Typeable)
newtype State    = State    PostState  deriving (Show, Eq, Typeable)
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

instance ToParameter Before   where mkParam (Before    d) = ("before",    fromString $ show $ toTimestamp d)
instance ToParameter BeforeId where mkParam (BeforeId  i) = ("before_id", fromString $ show i)
instance ToParameter Filter   where mkParam (Filter    f) = ("filter",    fromString $ show f)
instance ToParameter Format   where mkParam (Format    f) = ("format",    fromString $ show f)
instance ToParameter Limit    where mkParam (Limit     p) = ("limit",     fromString $ show $ clamp 1 20 p)
instance ToParameter Offset   where mkParam (Offset    o) = ("offset",    fromString $ show o)
instance ToParameter Title    where mkParam (Title     t) = ("title ",    fromString $ N.encode t)
instance ToParameter Tag      where mkParam (Tag       t) = ("tag",       fromString t)
instance ToParameter State    where mkParam (State     s) = ("state",     fromString $ show s)
instance ToParameter PRange   where mkParam (POffset   o) = ("offset",    fromString $ show o)
                                    mkParam (PBefore   d) = ("before",    fromString $ show $ toTimestamp d)
                                    mkParam (PAfter    d) = ("after",     fromString $ show $ toTimestamp d)


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

instance QueryInfo  'GetBlogInfo where
  type QueryResult  'GetBlogInfo = Blog
  getMethod = const QGet

getBlogInfo :: MonadMaybeAuth c m => BlogId -> m (Query 'GetBlogInfo m)
getBlogInfo (BlogId bid) = liftMaybeAddAuth $ mkQuery bid "/info" []

getInfo :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'GetBlogInfo m)
getInfo = asks getBlogId >>= getBlogInfo



-- blog avatar

instance QueryInfo  'GetBlogAvatar where
  type QueryResult  'GetBlogAvatar = PNGImage
  getMethod = const QGet

getBlogAvatar :: MonadMaybeAuth c m => BlogId -> AvatarSize -> m (Query 'GetBlogAvatar m)
getBlogAvatar (BlogId bid) s = liftMaybeAddAuth $ mkQuery bid ("/avatar/" ++ show s) []

getAvatar :: (MonadMaybeAuth c m, HasBlogId c) => AvatarSize -> m (Query 'GetBlogAvatar m)
getAvatar s = asks getBlogId >>= flip getBlogAvatar s



-- blog likes

instance QueryParam 'GetBlogLikes Limit
instance QueryParam 'GetBlogLikes PRange
instance QueryInfo  'GetBlogLikes where
  type QueryResult  'GetBlogLikes = PostList
  getMethod = const QGet

getBlogLikes :: MonadMaybeAuth c m => BlogId -> m (Query 'GetBlogLikes m)
getBlogLikes (BlogId bid) = liftMaybeAddAuth $ mkQuery bid "/likes" []

getLikes :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'GetBlogLikes m)
getLikes = asks getBlogId >>= getBlogLikes



-- blog followees

instance QueryParam 'GetBlogFollowees Limit
instance QueryParam 'GetBlogFollowees Offset
instance QueryInfo  'GetBlogFollowees where
  type QueryResult  'GetBlogFollowees = BlogSummaryList
  getMethod = const QGet

getBlogFollowees :: MonadAuth c m => BlogId -> m (Query 'GetBlogFollowees m)
getBlogFollowees (BlogId bid) = liftAddAuth $ mkQuery bid "/following" []

getFollowees :: (MonadAuth c m, HasBlogId c) => m (Query 'GetBlogFollowees m)
getFollowees = asks getBlogId >>= getBlogFollowees



-- blog followers

instance QueryParam 'GetBlogFollowers Limit
instance QueryParam 'GetBlogFollowers Offset
instance QueryInfo  'GetBlogFollowers where
  type QueryResult  'GetBlogFollowers = BlogSummaryList
  getMethod = const QGet

getBlogFollowers :: MonadAuth c m => BlogId -> m (Query 'GetBlogFollowers m)
getBlogFollowers (BlogId bid) = liftAddAuth $ mkQuery bid "/followers" []

getFollowers :: (MonadAuth c m, HasBlogId c) => m (Query 'GetBlogFollowers m)
getFollowers = asks getBlogId >>= getBlogFollowers



-- blog post

instance QueryInfo 'GetBlogPost where
  type QueryResult 'GetBlogPost = PostList
  getMethod = const QGet

getBlogPost :: MonadMaybeAuth c m => BlogId -> PostId -> m (Query 'GetBlogPosts m)
getBlogPost (BlogId bid) pid = liftMaybeAddAuth $ mkQuery bid "/posts" qs
  where qs = [("id", fromString $ show pid)]

getPost :: (MonadMaybeAuth c m, HasBlogId c) => PostId -> m (Query 'GetBlogPosts m)
getPost pid = asks getBlogId >>= flip getBlogPost pid



-- blog posts

instance QueryParam 'GetBlogPosts Before
instance QueryParam 'GetBlogPosts Limit
instance QueryParam 'GetBlogPosts Offset
instance QueryParam 'GetBlogPosts Filter
instance QueryParam 'GetBlogPosts Tag
instance QueryInfo  'GetBlogPosts where
  type QueryResult  'GetBlogPosts = PostList
  getMethod = const QGet

getBlogPosts :: MonadMaybeAuth c m => BlogId -> m (Query 'GetBlogPosts m)
getBlogPosts (BlogId bid) = liftMaybeAddAuth $ mkQuery bid "/posts" []

getPosts :: (MonadMaybeAuth c m, HasBlogId c) => m (Query 'GetBlogPosts m)
getPosts = asks getBlogId >>= getBlogPosts

getBlogPostsByType :: MonadMaybeAuth c m => BlogId -> PostType -> m (Query 'GetBlogPosts m)
getBlogPostsByType (BlogId bid) t = liftMaybeAddAuth $ mkQuery bid ("/posts/" ++ show t) []

getPostsByType :: (MonadMaybeAuth c m, HasBlogId c) => PostType -> m (Query 'GetBlogPosts m)
getPostsByType t = asks getBlogId >>= flip getBlogPostsByType t



-- blog queued posts

instance QueryParam 'GetBlogQueuedPosts Limit
instance QueryParam 'GetBlogQueuedPosts Offset
instance QueryParam 'GetBlogQueuedPosts Filter
instance QueryInfo  'GetBlogQueuedPosts where
  type QueryResult  'GetBlogQueuedPosts = PostList
  getMethod = const QGet

getBlogQueuedPosts :: MonadAuth c m => BlogId -> m (Query 'GetBlogQueuedPosts m)
getBlogQueuedPosts (BlogId bid) = liftAddAuth $ mkQuery bid "/posts/queue" []

getQueuedPosts :: (MonadAuth c m, HasBlogId c) => m (Query 'GetBlogQueuedPosts m)
getQueuedPosts = asks getBlogId >>= getBlogQueuedPosts



-- blog draft posts

instance QueryParam 'GetBlogDraftPosts BeforeId
instance QueryParam 'GetBlogDraftPosts Filter
instance QueryInfo  'GetBlogDraftPosts where
  type QueryResult  'GetBlogDraftPosts = PostList
  getMethod = const QGet

getBlogDraftPosts :: MonadAuth c m => BlogId -> m (Query 'GetBlogDraftPosts m)
getBlogDraftPosts (BlogId bid) = liftAddAuth $ mkQuery bid "/posts/draft" []

getDraftPosts :: (MonadAuth c m, HasBlogId c) => m (Query 'GetBlogDraftPosts m)
getDraftPosts = asks getBlogId >>= getBlogDraftPosts



-- post new text

instance QueryParam 'PostText Format
instance QueryParam 'PostText Title
instance QueryParam 'PostText State
instance QueryInfo  'PostText where
  type QueryResult  'PostText = ()
  getMethod = const QPost

postNewBlogText :: MonadAuth c m => BlogId -> String -> m (Query 'PostText m)
postNewBlogText (BlogId bid) body = liftAddAuth $ mkQuery bid "/posts/draft" qs
  where qs = [ ("type", "text")
             , ("body", fromString $ N.encode body)
             ]

postNewText :: (MonadAuth c m, HasBlogId c) => String -> m (Query 'PostText m)
postNewText body = asks getBlogId >>= flip postNewBlogText body



-- local helpers

mkQuery :: (Monad m, QueryInfo q) => String -> String -> [Parameter] -> m (Query q m)
mkQuery bid path qs = return resultQuery
  where resultQuery = Query req M.empty return
        req = appendParams qs $
              N.defaultRequest { N.path   = fromString $ "/v2/blog/" ++ bid ++ path
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
