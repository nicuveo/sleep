{-# LANGUAGE OverloadedStrings #-}



-- module

module Web.Sleep.Tumblr.Data (
  -- helper types
  HTML,
  Tag,
  PNGImage(..),
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
  postFormat,
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

import           Control.Monad             (join)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8     as SB (unpack)
import qualified Data.ByteString.Lazy      as LB
import           Data.Text                 as T
import           Data.Time.Clock
import qualified Network.HTTP.Client       as N
import qualified Network.HTTP.Types.Header as N
import           Network.URL

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Network
import           Web.Sleep.Tumblr.Response



-- exported types

type HTML = String
type Tag = String

data PNGImage = ImageRawData LB.ByteString
              | ImageURL     URL

data PostFormat = HTMLPost
                | MarkdownPost
                deriving (Show, Eq, Enum, Bounded)

data PostState = PrivatePost
               | DraftPost
               | QueuedPost
               | PublishedPost
               deriving (Show, Eq, Enum, Bounded)

data PostType = AnswerType
              | AudioType
              | ChatType
              | LinkType
              | PhotoType
              | QuoteType
              | TextType
              | VideoType
              deriving (Eq, Enum, Bounded)

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
                         , pFormat      :: PostFormat
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

newtype PostList = PostList { postList :: [Post]
                            } deriving (Show, Eq)



-- exported functions

postType :: Post -> PostType
postType AnswerPost {} = AnswerType
postType AudioPost  {} = AudioType
postType ChatPost   {} = ChatType
postType LinkPost   {} = LinkType
postType PhotoPost  {} = PhotoType
postType QuotePost  {} = QuoteType
postType TextPost   {} = TextType
postType VideoPost  {} = VideoType

postId :: Post -> Int
postId = pId . postBase

postBlogName :: Post -> String
postBlogName = pBlogName . postBase

postBookmarklet :: Post -> Bool
postBookmarklet = pBookmarklet . postBase

postDate :: Post -> UTCTime
postDate = pDate . postBase

postFormat :: Post -> PostFormat
postFormat = pFormat . postBase

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
ifPresent = traverse

parsePostBase :: Object -> Parser PostBase
parsePostBase o = do
  theId       <- o .: "id"
  theBlogName <- o .: "blog_name"
  bookmarklet <- o .:? "bookmarklet" .!= False
  date        <- fromTimestamp <$> o .: "timestamp"
  format      <- o .: "format"
  liked       <- o .:? "liked"
  mobile      <- o .:? "mobile" .!= False
  noteCount   <- o .: "note_count"
  reblogKey   <- o .: "reblog_key"
  sourceTitle <- o .:? "source_title"
  sourceURL   <- ifPresent parseURL =<< o .:? "source_url"
  state       <- o .: "state"
  tags        <- o .: "tags"
  theURL      <- parseURL =<< o .: "post_url"
  return $ PostBase theId theBlogName bookmarklet date format liked mobile noteCount reblogKey sourceTitle sourceURL state tags theURL

postBaseToObject :: Post -> [Pair]
postBaseToObject p = join [ [ "id"             .= pId pb
                            , "blog_name"      .= pBlogName pb
                            , "bookmarklet"    .= pBookmarklet pb
                            , "timestamp"      .= toTimestamp (pDate pb)
                            , "format"         .= pFormat pb
                            , "mobile"         .= pMobile pb
                            , "note_count"     .= pNoteCount pb
                            , "reblog_key"     .= pReblogKey pb
                            , "state"          .= pState pb
                            , "tags"           .= pTags pb
                            , "type"           .= postType p
                            , "post_url"       .= exportURL (pURL pb)
                            ]
                          , [ "liked"          .= liked
                            | Just liked       <- [pLiked pb]
                            ]
                          , [ "source_title"   .= sourceTitle
                            | Just sourceTitle <- [pSourceTitle pb]
                            ]
                          , [ "source_url"     .= exportURL sourceURL
                            | Just sourceURL   <- [pSourceURL pb]
                            ]
                          ]
  where pb = postBase p



-- instances

instance FromJSON PNGImage where
  parseJSON = withObject "avatar" $ \o -> fmap ImageURL $ parseURL =<< o .: "avatar_url"

instance ToJSON PNGImage where
  toJSON _ = error "[not implemented yet]"

instance Decode PNGImage where
  decode r = case lookup N.hContentType $ N.responseHeaders r of
               Just "image/png" -> Right $ ImageRawData $ N.responseBody r
               Just "text/json" -> getResponse $ N.responseBody r
               Just t           -> Left $ ClientError 2 $ "expected image type: " ++ SB.unpack t -- FIXME
               Nothing          -> Left $ ClientError 3 "missing content type header"            -- FIXME


instance FromJSON PostFormat where
  parseJSON = withText "post format" parseFormat
    where parseFormat "html"     = return HTMLPost
          parseFormat "markdown" = return MarkdownPost
          parseFormat s          = fail $ T.unpack s ++ " is not a valid post format"

instance ToJSON PostFormat where
  toJSON HTMLPost     = "html"
  toJSON MarkdownPost = "markdown"

instance Decode PostFormat


instance FromJSON PostState where
  parseJSON = withText "post state" parseState
    where parseState "private"   = return PrivatePost
          parseState "draft"     = return DraftPost
          parseState "queued"    = return QueuedPost
          parseState "published" = return PublishedPost
          parseState s           = fail $ T.unpack s ++ " is not a valid post state"

instance ToJSON PostState where
  toJSON PrivatePost   = "private"
  toJSON DraftPost     = "draft"
  toJSON QueuedPost    = "queued"
  toJSON PublishedPost = "published"

instance Decode PostState


instance Show PostType where
  show AnswerType = "answer"
  show AudioType  = "audio"
  show ChatType   = "chat"
  show LinkType   = "link"
  show PhotoType  = "photo"
  show QuoteType  = "quote"
  show TextType   = "text"
  show VideoType  = "video"

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

instance ToJSON PostType where
  toJSON AnswerType = "answer"
  toJSON AudioType  = "audio"
  toJSON ChatType   = "chat"
  toJSON LinkType   = "link"
  toJSON PhotoType  = "photo"
  toJSON QuoteType  = "quote"
  toJSON TextType   = "text"
  toJSON VideoType  = "video"

instance Decode PostType


instance FromJSON DialogueEntry where
  parseJSON = withObject "dialog entry" $ \o -> do
    name   <- o .: "name"
    label  <- o .: "label"
    phrase <- o .: "phrase"
    return $ DialogueEntry name label phrase

instance ToJSON DialogueEntry where
  toJSON (DialogueEntry name label phrase) =
    object [ "name"   .= name
           , "label"  .= label
           , "phrase" .= phrase
           ]

instance Decode DialogueEntry


instance FromJSON PhotoSize where
  parseJSON = withObject "photo size" $ \o -> do
    width  <- o .: "width"
    height <- o .: "height"
    url    <- parseURL =<< o .: "url"
    return $ PhotoSize width height url

instance ToJSON PhotoSize where
  toJSON (PhotoSize width height url) =
    object [ "width"  .= width
           , "height" .= height
           , "url"    .= exportURL url
           ]

instance Decode PhotoSize


instance FromJSON Photo where
  parseJSON = withObject "photo" $ \o -> do
    caption  <- o .:? "caption"
    original <- o .: "original_size"
    alt      <- o .: "alt_sizes"
    return $ Photo caption original alt

instance ToJSON Photo where
  toJSON (Photo mcaption original alt) =
    object $ [ "original_size" .= original
             , "alt_sizes"     .= alt
             ] ++ [ "caption"  .= capt
                  | Just capt <- [mcaption]
                  ]

instance Decode Photo


instance FromJSON Video where
  parseJSON = withObject "video" $ \o -> do
    width <- o .: "width"
    code  <- o .: "embed_code"
    return $ Video width code

instance ToJSON Video where
  toJSON (Video width code) = object [ "width"      .= width
                                     , "embed_code" .= code
                                     ]

instance Decode Video


instance FromJSON Blog where
  parseJSON = withObject "blog" $ \o -> do
    b       <- o .: "blog"
    title   <- b .: "title"
    name    <- b .: "name"
    desc    <- b .: "description"
    posts   <- b .: "posts"
    updated <- fromTimestamp <$> b .: "updated"
    ask     <- b .: "ask"
    likes   <- b .:? "likes"
    askAnon <- b .:? "ask_anon" .!= False
    blocked <- b .:? "is_blocked_from_primary" .!= False
    return $ Blog title name desc posts updated ask askAnon blocked likes

instance ToJSON Blog where
  toJSON b = object
    [ "blog" .= object (
        [ "title"                   .= blogTitle b
        , "name"                    .= blogName b
        , "description"             .= blogDescription b
        , "posts"                   .= blogPosts b
        , "updated"                 .= toTimestamp (blogUpdated b)
        , "ask"                     .= blogAsk b
        , "ask_anon"                .= blogAskAnon b
        , "is_blocked_from_primary" .= blogBlocked b
        ] ++ [ "likes"              .= likes
             | Just likes <- [blogLikes b]
             ])
    ]

instance Decode Blog


instance FromJSON PostBase where
  parseJSON = withObject "post base" parsePostBase

instance FromJSON Post where
  parseJSON = withObject "post" $ \o -> do
    base        <- parsePostBase o
    thePostType <- o .: "type"
    case thePostType of AnswerType -> parseAnswer base o
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

instance ToJSON Post where
  toJSON p = object $ postBaseToObject p ++ details p
    where details (AnswerPost _ answer askingName askingURL question) =
            [ "answer"      .= answer
            , "asking_name" .= askingName
            , "asking_url"  .= exportURL askingURL
            , "question"    .= question
            ]
          details (AudioPost _ mAlbum mAlbumArt mArtist caption player plays mTitle mTrackName mTrackNumber mYear) =
            join [ [ "caption"          .= caption
                   , "player"           .= player
                   , "plays"            .= plays
                   ]
                 , [ "id3_album"        .= album              | Just album        <- [mAlbum]       ]
                 , [ "id3_album_art"    .= exportURL albumArt | Just albumArt     <- [mAlbumArt]    ]
                 , [ "id3_artist"       .= artist             | Just artist       <- [mArtist]      ]
                 , [ "id3_title"        .= title              | Just title        <- [mTitle]       ]
                 , [ "id3_track_name"   .= track_name         | Just track_name   <- [mTrackName]   ]
                 , [ "id3_track_number" .= track_number       | Just track_number <- [mTrackNumber] ]
                 , [ "id3_year"         .= year               | Just year         <- [mYear]        ]
                 ]
          details (ChatPost _ dialogue mTitle) =
            join [ [ "dialogue" .= dialogue                          ]
                 , [ "title"    .= title    | Just title <- [mTitle] ]
                 ]
          details (LinkPost _ author description excerpt link photos publisher mTitle) =
            join [ [ "author"      .= author
                   , "description" .= description
                   , "excerpt"     .= excerpt
                   , "url"         .= exportURL link
                   , "photos"      .= photos
                   , "publisher"   .= publisher
                   ]
                 , [ "title"       .= title | Just title <- [mTitle] ]
                 ]
          details (PhotoPost _ caption photos) =
            [ "caption" .= caption
            , "photos"  .= photos
            ]
          details (QuotePost _ source text) =
            [ "source" .= source
            , "text"   .= text
            ]
          details (TextPost _ body mTitle) =
            join [ [ "body"  .= body                           ]
                 , [ "title" .= title | Just title <- [mTitle] ]
                 ]
          details (VideoPost _ caption player) =
            [ "caption" .= caption
            , "player"  .= player
            ]

instance Decode Post


instance FromJSON BlogList where
  parseJSON = withObject "blog list" $ \o ->
    BlogList <$> o .: "blogs"

instance ToJSON BlogList where
  toJSON (BlogList blogs) = object [ "blogs" .= blogs ]

instance Decode BlogList


instance FromJSON PostList where
  parseJSON = withObject "post list" $ \o ->
    PostList <$> o .: "posts"

instance ToJSON PostList where
  toJSON (PostList posts) = object [ "posts" .= posts ]

instance Decode PostList
