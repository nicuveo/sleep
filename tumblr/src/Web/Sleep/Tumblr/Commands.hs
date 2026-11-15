module Web.Sleep.Tumblr.Commands
  ( -- * Blog commands
    getBlogInfo
  , getBlogAvatar
    -- * Post commnads
  , GetPostsArgs (..)
  , getPostsArgs
  , getPosts
  , getPostById
  ) where

import "sleep-core" Prelude

import Data.Text                qualified as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.HTTP.Types

import Web.Sleep.Tumblr.Data
import Web.Sleep.Tumblr.Monad
import Web.Sleep.Tumblr.Request


--------------------------------------------------------------------------------
-- Blog commands

getBlogInfo :: (MonadAPIKeyTumblr m) => BlogId -> m Blog
getBlogInfo blogId = apiRequest $
  methodGet
    /. "/v2/blog/" <> blogId <> "/info"

getBlogAvatar :: (MonadTumblr m) => BlogId -> m PNGImage
getBlogAvatar blogId = request $
  methodGet
    /. "/v2/blog/" <> blogId <> "/avatar"



--------------------------------------------------------------------------------
-- Post commands

data SortOrder = Asc | Desc
  deriving (Eq, Show, Ord, Bounded, Enum)

data GetPostsArgs = GetPostsArgs
  { getPostsType   :: Maybe PostType
  , getPostsTags   :: [Text]
  , getPostsLimit  :: Int
  , getPostsOffset :: Int
  , getPostsBefore :: Maybe UTCTime
  , getPostsAfter  :: Maybe UTCTime
  , getPostsSort   :: SortOrder
  }
  deriving (Show)

getPostsArgs :: GetPostsArgs
getPostsArgs = GetPostsArgs
  { getPostsType   = Nothing
  , getPostsTags   = []
  , getPostsLimit  = 20
  , getPostsOffset = 0
  , getPostsBefore = Nothing
  , getPostsAfter  = Nothing
  , getPostsSort   = Desc
  }

getPosts :: (MonadAPIKeyTumblr m) => BlogId -> GetPostsArgs -> m [Post]
getPosts blogId GetPostsArgs {..} = apiRequest $ addTags $
  methodGet
    /. "/v2/blog/" <> blogId <> "/posts/" <> pType
    &.  ("limit",  Just $ show getPostsLimit)
    &.  ("offset", Just $ show getPostsOffset)
    &.  ("sort",   Just pSort)
    &?. ("before",) . renderTime <$> getPostsBefore
    &?. ("after",)  . renderTime <$> getPostsAfter
  where
    addTags = case getPostsTags of
      []    -> id
      [tag] -> (&. ("tag", Just tag))
      tags  -> flip (foldl' addTag) (zip [0..] tags)

    addTag req (i :: Int, tag) =
      req &. (Text.pack $ "tag[" ++ show i ++ "]", Just tag)

    pType = case getPostsType of
      Nothing         -> ""
      Just AnswerType -> "answer"
      Just AudioType  -> "audio"
      Just ChatType   -> "chat"
      Just LinkType   -> "link"
      Just PhotoType  -> "photo"
      Just QuoteType  -> "quote"
      Just TextType   -> "text"
      Just VideoType  -> "video"

    pSort = case getPostsSort of
      Asc  -> "asc"  :: Text
      Desc -> "desc" :: Text

    renderTime =
      show . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getPostById :: (MonadAPIKeyTumblr m) => BlogId -> PostId -> m Post
getPostById blogId pId = apiRequest $
  methodGet
    /. "/v2/blog/" <> blogId <> "/posts"
    &. ("id", Just $ show pId)
