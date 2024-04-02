module Web.Sleep.Tumblr.Commands
  ( cmd
  , getBlogInfo
  , getBlogAvatar
  ) where

import "sleep-core" Prelude

import Data.Aeson                  qualified as A
import Data.ByteString             qualified as B
import Data.Kind                   (Type)
import Network.HTTP.Client         qualified as N
import Network.HTTP.Types          (Method)

import Web.Sleep.Core.Command
import Web.Sleep.Core.Error
import Web.Sleep.Core.Request

import Web.Sleep.Tumblr.Auth
import Web.Sleep.Tumblr.Data
import Web.Sleep.Tumblr.Parameters
import Web.Sleep.Tumblr.Response


--------------------------------------------------------------------------------
-- Command class

class Commandable a where
  type Result a :: Type
  cmd :: a -> Command (Result a)


--------------------------------------------------------------------------------
-- Blog commands

-- blog info

data GetBlogInfo = GetBlogInfo
  { userAgent :: UserAgent
  , queryAuth :: APIKeyRequired
  , blogId    :: BlogId
  }

getBlogInfo :: UserAgent -> APIKeyRequired -> BlogId -> GetBlogInfo
getBlogInfo = GetBlogInfo

instance Commandable GetBlogInfo where
  type Result GetBlogInfo = Blog
  cmd (GetBlogInfo {..}) = Command
    { request = makeBlogRequest blogId "GET" "/info" userAgent queryAuth []
    , decode  = decodeJSON @Blog
    }

-- blog avatar

data GetBlogAvatar = GetBlogAvatar
  { avatarSize :: Maybe AvatarSize
  , userAgent  :: UserAgent
  , queryAuth  :: APIKeyRequired
  , blogId     :: BlogId
  }

getBlogAvatar :: UserAgent -> APIKeyRequired -> BlogId -> GetBlogAvatar
getBlogAvatar = GetBlogAvatar Nothing

instance Commandable GetBlogAvatar where
  type Result GetBlogAvatar = PNGImage
  cmd (GetBlogAvatar {..}) = Command
    { request = makeBlogRequest blogId "GET" ("/avatar" <> size) userAgent queryAuth []
    , decode  = decodeJSON @PNGImage
    }
    where
      size = case avatarSize of
        Nothing -> ""
        Just as -> "/" <> toText as

-- blog posts

{-
data GetBlogPosts = GetBlogPosts
  { userAgent :: UserAgent
  , queryAuth :: APIKeyRequired
  , blogId    :: BlogId
  , postType  :: PostType
  , postId    :: PostId
  }
-}


--------------------------------------------------------------------------------
-- Internal helpers

makeBlogRequest
  :: BlogId
  -> Method
  -> Text
  -> UserAgent
  -> AuthorizationLevel n k t
  -> [(B.ByteString, Maybe B.ByteString)]
  -> N.Request
makeBlogRequest blog method path userAgent auth qs =
  N.defaultRequest
    { N.method = method
    , N.host   = "api.tumblr.com"
    , N.path   = "/v2/blog/" <> toByteString blog <> toByteString path
    }
  & N.setQueryString qs
  & addHeader "User-Agent" (toByteString userAgent)
  & signRequest auth

decodeJSON
  :: A.FromJSON (Envelope a)
  => B.ByteString
  -> Either Error a
decodeJSON v = do
  envelope <- left toClientError $ A.eitherDecodeStrict v
  unwrap envelope
  where
    toClientError = ClientError 1 . fromString
