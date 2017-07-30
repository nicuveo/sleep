{-# LANGUAGE OverloadedStrings #-}



-- module

module Web.Sleep.Tumblr.Data (
  -- helper types
  HTML,
  Tag,
  -- data types
  PostFormat(..),
  PostState(..),
  PostType(..),
  DialogueEntry(..),
  PhotoSize(..),
  Photo(..),
  Video(..),
  Blog(..),
  PostBase(..),
  Post(..),
  BlogList(..),
  PostList(..),
  -- accessors
  postBlogName,
  postBookmarklet,
  postDate,
  postFornat,
  postId,
  postLiked,
  postMobile,
  postNoteCount,
  postReblogKey,
  postSourceTitle,
  postSourceURL,
  postState,
  postTags,
  postType,
  postURL,
  ) where



-- imports

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map              as M
import           Data.Text             as T
import           Data.Time.Clock
import           Network.URL

import           Web.Sleep.Common.Misc



-- exported types

type HTML = String
type Tag = String

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
              deriving Eq

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


data Post = AnswerPost { postBase       :: PostBase
                       , postAnswer     :: String
                       , postAskingName :: String
                       , postAskingURL  :: URL
                       , postQuestion   :: String
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
          | ChatPost   { postBase     :: PostBase
                       , postDialogue :: [DialogueEntry]
                       , postTitle    :: Maybe String
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
          | PhotoPost  { postBase    :: PostBase
                       , postCaption :: String
                       , postPhotos  :: [Photo]
                       }
          | QuotePost  { postBase   :: PostBase
                       , postSource :: HTML
                       , postText   :: String
                       }
          | TextPost   { postBase  :: PostBase
                       , postBody  :: HTML
                       , postTitle :: Maybe String
                       }
          | VideoPost  { postBase    :: PostBase
                       , postCaption :: String
                       , postPlayers :: [Video]
                       }
          deriving (Show, Eq)

newtype BlogList = BlogList { blogList :: [Blog]
                            } deriving (Show, Eq)

data PostList = PostList { postBlog :: Blog
                         , postList :: [Post]
                         } deriving (Show, Eq)



-- exported functions

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



-- internal functions

parseURL :: String -> Parser URL
parseURL s = maybe (fail msg) return $ importURL s
  where msg = s ++ " is not a valid url"

ifPresent :: (a -> Parser b) -> Maybe a -> Parser (Maybe b)
ifPresent p = sequence . fmap p

parsePostBase :: Object -> Parser PostBase
parsePostBase o = do
  id          <- o .: "id"
  blogName    <- o .: "blog_name"
  bookmarklet <- o .:? "bookmarklet" .!= False
  date        <- fromTimestamp <$> o .: "timestamp"
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



-- instances

instance Show PostType where
  show AnswerType = "answer"
  show AudioType  = "audio"
  show ChatType   = "chat"
  show LinkType   = "link"
  show PhotoType  = "photo"
  show QuoteType  = "quote"
  show TextType   = "text"
  show VideoType  = "video"

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

instance FromJSON Blog where
  parseJSON = withObject "blog" $ \o -> do
    title   <- o .: "title"
    name    <- o .: "name"
    desc    <- o .: "description"
    posts   <- o .: "posts"
    updated <- fromTimestamp <$> o .: "updated"
    ask     <- o .: "ask"
    likes   <- o .:? "likes"
    askAnon <- o .:? "ask_anon" .!= False
    blocked <- o .:? "is_blocked_from_primary" .!= False
    return $ Blog title name desc posts updated ask askAnon blocked likes

instance FromJSON PostBase where
  parseJSON = withObject "post base" parsePostBase

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

instance FromJSON BlogList where
  parseJSON = withObject "blog list" $ \o -> do
    BlogList <$> o .: "blogs"

instance FromJSON PostList where
  parseJSON = withObject "post list" $ \o -> do
    blog <- o .: "blog"
    list <- o .: "posts"
    return $ PostList blog list
