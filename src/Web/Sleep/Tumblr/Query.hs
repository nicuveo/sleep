{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}



-- module

module Web.Sleep.Tumblr.Query where



-- imports

import           Data.Aeson
import qualified Data.ByteString           as B (ByteString)
import qualified Data.ByteString.Lazy      as LB (ByteString)
import qualified Data.Map.Strict           as M
import           Data.String
import           Data.Time.Clock
import           Data.Typeable
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Method as N
import qualified Network.URI.Encode        as N

import           Web.Sleep.Common.Misc
import           Web.Sleep.Libs.Request
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- query

data QueryName = GetBlogInfo
               | GetBlogAvatar
               | GetBlogLikes
               | GetBlogFollowees
               | GetBlogFollowers
               | GetPost
               | GetPosts
               | GetQueuedPosts
               | GetDraftPosts
               | PostText

data Query (q :: QueryName) = Query { baseRequest :: N.Request
                                    , params      :: ParametersMap
                                    }

class QueryMethod q where
  getMethod :: Query q -> N.Method



-- decode

class Decode q r | q -> r where
  decode :: Query q -> N.Response LB.ByteString -> Either Error r
  default decode :: FromJSON (Envelope r) => Query q -> N.Response LB.ByteString -> Either Error r
  decode = const decodeJSON



-- create request

type OAuthFunction m = N.Request -> m N.Request

class OAuthCommand q where
  mkOAuthRequest :: OAuthFunction m -> APIKey -> Query q -> m N.Request
  mkOAuthRequest = mkOAuthReq

class OAuthCommand q => APIKeyCommand q where
  mkAPIKeyRequest :: APIKey -> Query q -> N.Request
  mkAPIKeyRequest = mkAPIKeyReq

class APIKeyCommand q => Command q where
  mkRequest :: Query q -> N.Request
  mkRequest = mkReq



-- query instance and helpers

instance Show (Query q) where
  show = show . N.getUri . mkReq

mkReq :: Query q -> N.Request
mkReq q = if | N.method req == N.methodGet  -> req
             | N.method req == N.methodPost ->
                 appendHeader N.hContentType "application/x-www-form-urlencoded" $
                 setBody (N.queryString req) $
                 N.setQueryString [] req
             | otherwise                    -> error "mkReq: unsupported method type"
  where req = appendParams (M.elems $ params q) $ baseRequest q

mkAPIKeyReq :: APIKey -> Query q -> N.Request
mkAPIKeyReq k = appendParam ("api_key", k) . mkReq

mkOAuthReq :: OAuthFunction m -> APIKey -> Query q -> m N.Request
mkOAuthReq = (... mkAPIKeyReq)



-- parameters

type Parameter = (B.ByteString, B.ByteString)
type ParametersMap = M.Map TypeRep Parameter

class Typeable p => ToParameter p where
  mkParam :: p -> Parameter

class ToParameter p => QueryParam (q :: QueryName) p where
  pAdd :: p -> Query q -> Query q
  pAdd p q = q { params = M.insert (typeOf p) (mkParam p) $ params q }

(&=) :: QueryParam q p => Query q -> p -> Query q
(&=) = flip pAdd
infixl 8 &=

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
instance ToParameter Title    where mkParam (Title     t) = ("title",     fromString $ N.encode t)
instance ToParameter Tag      where mkParam (Tag       t) = ("tag",       fromString t)
instance ToParameter State    where mkParam (State     s) = ("state",     fromString $ show s)
instance ToParameter PRange   where mkParam (POffset   o) = ("offset",    fromString $ show o)
                                    mkParam (PBefore   d) = ("before",    fromString $ show $ toTimestamp d)
                                    mkParam (PAfter    d) = ("after",     fromString $ show $ toTimestamp d)



-- blog info

instance QueryMethod   'GetBlogInfo where getMethod = const N.methodGet
instance OAuthCommand  'GetBlogInfo
instance APIKeyCommand 'GetBlogInfo
instance Decode        'GetBlogInfo Blog

getBlogInfo :: BlogId -> Query 'GetBlogInfo
getBlogInfo (BlogId bid) = mkQuery bid "/info" []



-- blog avatar

instance QueryMethod   'GetBlogAvatar where getMethod = const N.methodGet
instance OAuthCommand  'GetBlogAvatar
instance APIKeyCommand 'GetBlogAvatar
instance Command       'GetBlogAvatar
instance Decode        'GetBlogAvatar PNGImage where decode = const decodePNG

getBlogAvatar :: BlogId -> AvatarSize -> Query 'GetBlogAvatar
getBlogAvatar (BlogId bid) s = mkQuery bid ("/avatar/" ++ show s) []



-- blog likes

instance QueryMethod   'GetBlogLikes where getMethod = const N.methodGet
instance OAuthCommand  'GetBlogLikes
instance APIKeyCommand 'GetBlogLikes
instance Decode        'GetBlogLikes PostList
instance QueryParam    'GetBlogLikes Limit
instance QueryParam    'GetBlogLikes PRange

getBlogLikes :: BlogId -> Query 'GetBlogLikes
getBlogLikes (BlogId bid) = mkQuery bid "/likes" []



-- blog followees

instance QueryMethod  'GetBlogFollowees where getMethod = const N.methodGet
instance OAuthCommand 'GetBlogFollowees
instance Decode       'GetBlogFollowees BlogSummaryList
instance QueryParam   'GetBlogFollowees Limit
instance QueryParam   'GetBlogFollowees Offset

getBlogFollowees :: BlogId -> Query 'GetBlogFollowees
getBlogFollowees (BlogId bid) = mkQuery bid "/following" []



-- blog followers

instance QueryMethod  'GetBlogFollowers where getMethod = const N.methodGet
instance OAuthCommand 'GetBlogFollowers
instance Decode       'GetBlogFollowers BlogSummaryList
instance QueryParam   'GetBlogFollowers Limit
instance QueryParam   'GetBlogFollowers Offset

getBlogFollowers :: BlogId -> Query 'GetBlogFollowers
getBlogFollowers (BlogId bid) = mkQuery bid "/followers" []



-- blog post

instance QueryMethod   'GetPost where getMethod = const N.methodGet
instance OAuthCommand  'GetPost
instance APIKeyCommand 'GetPost
instance Decode        'GetPost PostList

getPost :: BlogId -> PostId -> Query 'GetPost
getPost (BlogId bid) pid = mkQuery bid "/posts" qs
  where qs = [("id", fromString $ show pid)]



-- blog posts

instance QueryMethod   'GetPosts where getMethod = const N.methodGet
instance OAuthCommand  'GetPosts
instance APIKeyCommand 'GetPosts
instance Decode        'GetPosts PostList
instance QueryParam    'GetPosts Before
instance QueryParam    'GetPosts Limit
instance QueryParam    'GetPosts Offset
instance QueryParam    'GetPosts Filter
instance QueryParam    'GetPosts Tag

getPosts :: BlogId -> Query 'GetPosts
getPosts (BlogId bid) = mkQuery bid "/posts" []

getPostsByType :: BlogId -> PostType -> Query 'GetPosts
getPostsByType (BlogId bid) t = mkQuery bid ("/posts/" ++ show t) []



-- blog queued posts

instance QueryMethod  'GetQueuedPosts where getMethod = const N.methodGet
instance OAuthCommand 'GetQueuedPosts
instance Decode       'GetQueuedPosts PostList
instance QueryParam   'GetQueuedPosts Limit
instance QueryParam   'GetQueuedPosts Offset
instance QueryParam   'GetQueuedPosts Filter

getQueuedPosts :: BlogId -> Query 'GetQueuedPosts
getQueuedPosts (BlogId bid) = mkQuery bid "/posts/queue" []



-- blog draft posts

instance QueryMethod  'GetDraftPosts where getMethod = const N.methodGet
instance OAuthCommand 'GetDraftPosts
instance Decode       'GetDraftPosts PostList
instance QueryParam   'GetDraftPosts BeforeId
instance QueryParam   'GetDraftPosts Filter

getDraftPosts :: BlogId -> Query 'GetDraftPosts
getDraftPosts (BlogId bid) = mkQuery bid "/posts/draft" []



-- post new text

instance QueryMethod  'PostText where getMethod = const N.methodPost
instance OAuthCommand 'PostText
instance Decode       'PostText ()
instance QueryParam   'PostText Format
instance QueryParam   'PostText Title
instance QueryParam   'PostText State

postNewText :: BlogId -> String -> Query 'PostText
postNewText (BlogId bid) b = mkQuery bid "/post" qs
  where qs = [("type", "text"), ("body", fromString b)]



-- local helpers

mkQuery :: QueryMethod q => String -> String -> [Parameter] -> Query q
mkQuery bid path qs = resultQuery
  where resultQuery = Query req M.empty
        req = appendParams qs $
              N.defaultRequest { N.path   = fromString $ "/v2/blog/" ++ bid ++ path
                               , N.method = getMethod resultQuery
                               , N.host   = "api.tumblr.com"
                               }
