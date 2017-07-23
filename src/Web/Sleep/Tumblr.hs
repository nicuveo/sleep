{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ViewPatterns           #-}



-- module

module Web.Sleep.Tumblr () where



-- imports

import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy
import qualified Data.Map                   as M
import           Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Network.URL

import           Network.HTTP.Conduit       hiding (Proxy)




-- response parsing

data Meta = Meta { metaStatus :: Int
                 , metaMsg    :: String
                 } deriving (Show)

data Envelope a = Envelope { envMeta :: Meta
                           , envResp :: a
                           } deriving (Show)

instance FromJSON Meta where
  parseJSON = withObject "envelope meta" $ \o -> do
    s <- o .: "status"
    m <- o .: "msg"
    return $ Meta s m

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = withObject "envelope" $ \o -> do
    m <- o .: "meta"
    r <- o .: "response"
    return $ Envelope m r

type RawData = ByteString
type ErrorMsg = String

isOk :: Envelope a -> Bool
isOk (metaStatus . envMeta -> status) = status == 200

getResponse :: FromJSON a => RawData -> Either ErrorMsg a
getResponse rd = do
  env <- eitherDecode' rd
  if isOk env
    then Right $ envResp env
    else Left $ metaMsg $ envMeta env

getResponseM :: (MonadError ErrorMsg m, FromJSON a) => RawData -> m a
getResponseM = either throwError return . getResponse



-- blog info

type HTML = String
type Tag = String

data Blog = Blog { blogTitle       :: String
                 , blogName        :: String
                 , blogDescription :: String
                 , blogPosts       :: Int
                 , blogUpdated     :: UTCTime
                 , blogAsk         :: Bool
                 , blogAskAnon     :: Bool
                 , blogBlocked     :: Bool
                 , blogLikes       :: Maybe Int
                 } deriving (Show, Eq)

data PostFormat = HTMLPost
                | MarkdownPost
                deriving (Show, Eq)

data PostState = PrivatePost
               | DraftPost
               | QueuedPost
               | PublishedPost
               deriving (Show, Eq)

data PostType = AnswerType
              | AudioType
              | ChatType
              | LinkType
              | PhotoType
              | QuoteType
              | TextType
              | VideoType
              deriving (Show, Eq)

data PostBase = PostBase { pId          :: Int
                         , pBlogName    :: String
                         , pBookmarklet :: Bool
                         , pDate        :: UTCTime
                         , pFornat      :: PostFormat
                         , pLiked       :: Maybe Bool
                         , pMobile      :: Bool
                         , pNoteCount   :: Int
                         , pReblogKey   :: String
                         , pSourceTitle :: Maybe String
                         , pSourceURL   :: Maybe URL
                         , pState       :: PostState
                         , pTags        :: [Tag]
                         , pURL         :: URL
                         } deriving (Show, Eq)

data DialogueEntry = DialogueEntry { entryName   :: String
                                   , entryLabel  :: String
                                   , entryPhrase :: String
                                   } deriving (Show, Eq)

data PhotoSize = PhotoSize { photoWidth  :: Int
                           , photoHeight :: Int
                           , photoURL    :: URL
                           } deriving (Show, Eq)

data Photo = Photo { photoCaption  :: Maybe String
                   , photoOriginal :: PhotoSize
                   , photoAltSizes :: [PhotoSize]
                   } deriving (Show, Eq)

data Video = Video { videoWidth :: Int
                   , videoCode  :: HTML
                   } deriving (Show, Eq)

data Post = AnswerPost { postBase        :: PostBase
                       , postAnswer      :: String
                       , postAskingName  :: String
                       , postAskingURL   :: URL
                       , postQuestion    :: String
                       }
          | AudioPost  { postBase        :: PostBase
                       , postAlbum       :: Maybe String
                       , postAlbumArt    :: Maybe URL
                       , postArtist      :: Maybe String
                       , postCaption     :: String
                       , postPlayer      :: HTML
                       , postPlays       :: Int
                       , postTitle       :: Maybe String
                       , postTrackName   :: Maybe String
                       , postTrackNumber :: Maybe Int
                       , postYear        :: Maybe Int
                       }
          | ChatPost   { postBase        :: PostBase
                       , postDialogue    :: [DialogueEntry]
                       , postTitle       :: Maybe String
                       }
          | LinkPost   { postBase        :: PostBase
                       , postAuthor      :: String
                       , postDescription :: String
                       , postExcerpt     :: String
                       , postLink        :: URL
                       , postPhotos      :: [Photo]
                       , postPublisher   :: String
                       , postTitle       :: Maybe String
                       }
          | PhotoPost  { postBase        :: PostBase
                       , postCaption     :: String
                       , postPhotos      :: [Photo]
                       }
          | QuotePost  { postBase        :: PostBase
                       , postSource      :: HTML
                       , postText        :: String
                       }
          | TextPost   { postBase        :: PostBase
                       , postBody        :: HTML
                       , postTitle       :: Maybe String
                       }
          | VideoPost  { postBase        :: PostBase
                       , postCaption     :: String
                       , postPlayers     :: [Video]
                       }
          deriving (Show, Eq)

data PostList = PostList { postBlog :: Blog
                         , postList :: [Post]
                         } deriving (Show, Eq)

postType :: Post -> PostType
postType (AnswerPost {}) = AnswerType
postType (AudioPost  {}) = AudioType
postType (ChatPost   {}) = ChatType
postType (LinkPost   {}) = LinkType
postType (PhotoPost  {}) = PhotoType
postType (QuotePost  {}) = QuoteType
postType (TextPost   {}) = TextType
postType (VideoPost  {}) = VideoType


postId :: Post -> Int
postId = pId . postBase

postBlogName :: Post -> String
postBlogName = pBlogName . postBase

postBookmarklet :: Post -> Bool
postBookmarklet = pBookmarklet . postBase

postDate :: Post -> UTCTime
postDate = pDate . postBase

postFornat :: Post -> PostFormat
postFornat = pFornat . postBase

postLiked :: Post -> Maybe Bool
postLiked = pLiked . postBase

postMobile :: Post -> Bool
postMobile = pMobile . postBase

postNoteCount :: Post -> Int
postNoteCount = pNoteCount . postBase

postReblogKey :: Post -> String
postReblogKey = pReblogKey . postBase

postSourceTitle :: Post -> Maybe String
postSourceTitle = pSourceTitle . postBase

postSourceURL :: Post -> Maybe URL
postSourceURL = pSourceURL . postBase

postState :: Post -> PostState
postState = pState . postBase

postTags :: Post -> [Tag]
postTags = pTags . postBase

postURL :: Post -> URL
postURL = pURL . postBase


parseURL :: String -> Parser URL
parseURL s = maybe (fail msg) return $ importURL s
  where msg = s ++ " is not a valid url"

ifPresent :: (a -> Parser b) -> Maybe a -> Parser (Maybe b)
ifPresent p = sequence . fmap p

parseUTCTime :: Integer -> UTCTime
parseUTCTime = posixSecondsToUTCTime . realToFrac . secondsToDiffTime

parsePostBase :: Object -> Parser PostBase
parsePostBase o = do
  id          <- o .: "id"
  blogName    <- o .: "blog_name"
  bookmarklet <- o .:? "bookmarklet" .!= False
  date        <- parseUTCTime <$> o .: "timestamp"
  fornat      <- o .: "format"
  liked       <- o .:? "liked"
  mobile      <- o .:? "mobile" .!= False
  noteCount   <- o .: "note_count"
  reblogKey   <- o .: "reblog_key"
  sourceTitle <- o .:? "source_title"
  sourceURL   <- ifPresent parseURL =<< o .:? "source_url"
  state       <- o .: "state"
  tags        <- o .: "tags"
  pURL        <- parseURL =<< o .: "post_url"
  return $ PostBase id blogName bookmarklet date fornat liked mobile noteCount reblogKey sourceTitle sourceURL state tags pURL

instance FromJSON Blog where
  parseJSON = withObject "blog" $ \o -> do
    title   <- o .: "title"
    name    <- o .: "name"
    desc    <- o .: "description"
    posts   <- o .: "posts"
    updated <- parseUTCTime<$> o .: "updated"
    ask     <- o .: "ask"
    likes   <- o .:? "likes"
    askAnon <- o .:? "ask_anon" .!= False
    blocked <- o .:? "is_blocked_from_primary" .!= False
    return $ Blog title name desc posts updated ask askAnon blocked likes

instance FromJSON PostFormat where
  parseJSON = withText "post format" parseFormat
    where parseFormat "html"     = return HTMLPost
          parseFormat "markdown" = return MarkdownPost
          parseFormat s          = fail $ T.unpack s ++ " is not a valid post format"

instance FromJSON PostState where
  parseJSON = withText "post state" parseState
    where parseState "private"   = return PrivatePost
          parseState "draft"     = return DraftPost
          parseState "queued"    = return QueuedPost
          parseState "published" = return PublishedPost
          parseState s           = fail $ T.unpack s ++ " is not a valid post state"

instance FromJSON PostType where
  parseJSON = withText "post type" parseType
    where parseType "answer" = return AnswerType
          parseType "audio"  = return AudioType
          parseType "chat"   = return ChatType
          parseType "link"   = return LinkType
          parseType "photo"  = return PhotoType
          parseType "quote"  = return QuoteType
          parseType "text"   = return TextType
          parseType "video"  = return VideoType
          parseType s        = fail $ T.unpack s ++ " is not a valid post type"

instance FromJSON PostBase where
  parseJSON = withObject "post base" parsePostBase

instance FromJSON DialogueEntry where
  parseJSON = withObject "dialog entry" $ \o -> do
    name   <- o .: "name"
    label  <- o .: "label"
    phrase <- o .: "phrase"
    return $ DialogueEntry name label phrase

instance FromJSON PhotoSize where
  parseJSON = withObject "photo size" $ \o -> do
    width  <- o .: "width"
    height <- o .: "height"
    url    <- parseURL =<< o .: "url"
    return $ PhotoSize width height url

instance FromJSON Photo where
  parseJSON = withObject "photo" $ \o -> do
    caption  <- o .:? "caption"
    original <- o .: "original_size"
    alt      <- o .: "alt_sizes"
    return $ Photo caption original alt

instance FromJSON Video where
  parseJSON = withObject "video" $ \o -> do
    width <- o .: "width"
    code  <- o .: "embed_code"
    return $ Video width code

instance FromJSON PostList where
  parseJSON = withObject "post list" $ \o -> do
    blog <- o .: "blog"
    list <- o .: "posts"
    return $ PostList blog list

instance FromJSON Post where
  parseJSON = withObject "post" $ \o -> do
    base     <- parsePostBase o
    postType <- o .: "type"
    case postType of AnswerType -> parseAnswer base o
                     AudioType  -> parseAudio  base o
                     ChatType   -> parseChat   base o
                     LinkType   -> parseLink   base o
                     PhotoType  -> parsePhoto  base o
                     QuoteType  -> parseQuote  base o
                     TextType   -> parseText   base o
                     VideoType  -> parseVideo  base o
    where parseAnswer base o = do
            answer      <- o .: "answer"
            askingName  <- o .: "asking_name"
            askingURL   <- parseURL =<< o .: "asking_url"
            question    <- o .: "question"
            return $ AnswerPost base answer askingName askingURL question
          parseAudio base o = do
            caption     <- o .: "caption"
            player      <- o .: "player"
            plays       <- o .: "plays"
            album       <- o .:? "id3_album"
            albumArt    <- ifPresent parseURL =<< o .:? "id3_album_art"
            artist      <- o .:? "id3_artist"
            title       <- o .:? "id3_title"
            trackName   <- o .:? "id3_track_name"
            trackNumber <- o .:? "id3_track_number"
            year        <- o .:? "id3_year"
            return $ AudioPost base album albumArt artist caption player plays title trackName trackNumber year
          parseChat base o = do
            dialogue    <- o .: "dialogue"
            title       <- o .:? "title"
            return $ ChatPost base dialogue title
          parseLink base o = do
            author      <- o .: "author"
            description <- o .: "description"
            excerpt     <- o .: "excerpt"
            link        <- parseURL =<< o .: "url"
            photos      <- o .: "photos"
            publisher   <- o .: "publisher"
            title       <- o .:? "title"
            return $ LinkPost base author description excerpt link photos publisher title
          parsePhoto base o = do
            caption     <- o .: "caption"
            photos      <- o .: "photos"
            return $ PhotoPost base caption photos
          parseQuote base o = do
            source      <- o .: "source"
            text        <- o .: "text"
            return $ QuotePost base source text
          parseText base o = do
            body        <- o .: "body"
            title       <- o .:? "title"
            return $ TextPost base body title
          parseVideo base o = do
            caption     <- o .: "caption"
            player      <- o .: "player"
            return $ VideoPost base caption player



-- request building

apiRoot = "https://api.tumblr.com/v2/"

type Parameter  = (String, String)
type Parameters = M.Map TypeRep Parameter

data Query q r = Query { method :: String
                       , params :: Parameters
                       } deriving Show

class ToParameter p where
  mkParam :: p -> Parameter

class QueryBuild q p where
  (&=) :: (Typeable p, ToParameter p) => Query q r -> p -> Query q r
  q &= p = q { params = addParam p $ params q }


-- parameters

newtype BlogId = BlogId String deriving (Show, Eq, Typeable)
newtype ApiKey = ApiKey String deriving (Show, Eq, Typeable)
newtype Limit  = Limit Int     deriving (Show, Eq, Typeable)
data PostRange = Offset Int
               | Before UTCTime
               | After  UTCTime
               deriving (Show, Eq, Typeable)

instance ToParameter BlogId    where mkParam (BlogId p) = ("blog-identifier", p)
instance ToParameter ApiKey    where mkParam (ApiKey p) = ("api-key", p)
instance ToParameter Limit     where mkParam (Limit  p) = ("limit", show $ clamp 1 20 p)
instance ToParameter PostRange where mkParam (Offset o) = ("offset", show o)
                                     mkParam (Before d) = ("before", show $ timestamp d)
                                     mkParam (After  d) = ("after",  show $ timestamp d)

addParam p = M.insert (typeOf p) $ mkParam p
clamp a b = max a . min b
timestamp = round . utcTimeToPOSIXSeconds


-- info query

data QInfo;

infoQuery :: ApiKey -> BlogId -> Query QInfo Blog
infoQuery key (BlogId bid) = Query method params
  where method = apiRoot ++ "blog/" ++ bid
        params = addParam key M.empty


-- likes query

data QLikes;
class QLikesParam p;
instance QLikesParam Limit;
instance QLikesParam PostRange;
instance QLikesParam p => QueryBuild QLikes p;

likesQuery :: ApiKey -> BlogId -> Query QLikes PostList
likesQuery key (BlogId bid) = Query method params
  where method = apiRoot ++ "likes/" ++ bid
        params = addParam key M.empty



-- debug

test :: IO ()
test = do
  res <- simpleHttp "https://api.tumblr.com/v2/blog/beesandbombs.tumblr.com/posts/photo?api_key=fuiKNFp9vQFvjLNvx4sUwti4Yb5yGutBN4Xh10LXZhhRKjWlV4"
  let q = likesQuery (ApiKey "") (BlogId "") &= (Offset 42)
  print q
  case getResponse res of
    Left  error   -> print $ "failed: " ++ error
    Right payload -> do
      print $ blogTitle $ postBlog payload
      sequence_ $ Prelude.putStrLn . exportURL . photoURL . photoOriginal <$> (postPhotos =<< postList payload)
