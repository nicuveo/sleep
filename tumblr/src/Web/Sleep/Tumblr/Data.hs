{-# LANGUAGE TemplateHaskell #-}

module Web.Sleep.Tumblr.Data where

import "sleep-core" Prelude

import Control.Lens          hiding (List)
import Data.Aeson
import Data.Aeson.Types
-- import Data.ByteString.Char8     qualified as SB (unpack)
import Data.ByteString.Lazy  qualified as LB
-- import Data.String
import Data.Text             qualified as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
-- import Network.HTTP.Client       qualified as N
-- import Network.HTTP.Types.Header qualified as N
import Network.URI           qualified as N

-- import Web.Sleep.Core.Error
-- import Web.Sleep.Tumblr.Response


--------------------------------------------------------------------------------
-- * Helper types

type HTML = Text

type PostId = Int

newtype Tag = Tag
  { getTag :: Text
  } deriving (Show, Eq)

data PNGImage
  = ImageRawData LB.ByteString
  | ImageURI     N.URI
  deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Tumblr data types

data PostFormat
  = HTMLPost
  | MarkdownPost
  deriving (Show, Eq, Enum, Bounded)

data PostState
  = PrivatePost
  | DraftPost
  | QueuedPost
  | PublishedPost
  deriving (Show, Eq, Enum, Bounded)

data PostType
  = AnswerType
  | AudioType
  | ChatType
  | LinkType
  | PhotoType
  | QuoteType
  | TextType
  | VideoType
  deriving (Show, Eq, Enum, Bounded)

data DialogueEntry = DialogueEntry
  { _entryName   :: Text
  , _entryLabel  :: Text
  , _entryPhrase :: Text
  } deriving (Show, Eq)

data PhotoSize = PhotoSize
  { _photoWidth  :: Int
  , _photoHeight :: Int
  , _photoURI    :: N.URI
  } deriving (Show, Eq)

data Photo = Photo
  { _photoCaption  :: Maybe Text
  , _photoOriginal :: PhotoSize
  , _photoAltSizes :: [PhotoSize]
  } deriving (Show, Eq)

data Video = Video
  { _videoWidth :: Int
  , _videoCode  :: HTML
  } deriving (Show, Eq)

type BlogId = Text

data Blog = Blog
  { _blogTitle       :: Text
  , _blogName        :: Text
  , _blogDescription :: Text
  , _blogPosts       :: Int
  , _blogUpdated     :: UTCTime
  , _blogAsk         :: Bool
  , _blogAskAnon     :: Bool
  , _blogBlocked     :: Bool
  , _blogLikes       :: Maybe Int
  } deriving (Show, Eq)

data BlogSummary = BlogSummary
  { _summaryName    :: Text
  , _summaryURI     :: N.URI
  , _summaryUpdated :: UTCTime
  } deriving (Show, Eq)

data PostCommonData = PostCommonData
  { _commonId          :: PostId
  , _commonBlogName    :: Text
  , _commonBookmarklet :: Bool
  , _commonDate        :: UTCTime
  , _commonFormat      :: PostFormat
  , _commonLiked       :: Maybe Bool
  , _commonMobile      :: Bool
  , _commonNoteCount   :: Int
  , _commonReblogKey   :: Text
  , _commonSourceTitle :: Maybe Text
  , _commonSourceURI   :: Maybe N.URI
  , _commonState       :: PostState
  , _commonTags        :: [Tag]
  , _commonURI         :: N.URI
  } deriving (Show, Eq)

data AnswerPostData = AnswerPostData
  { _answerCommon     :: PostCommonData
  , _answerText       :: Text
  , _answerAskingName :: Text
  , _answerAskingURI  :: N.URI
  , _answerQuestion   :: Text
  } deriving (Show, Eq)

data AudioPostData = AudioPostData
  { _audioCommon      :: PostCommonData
  , _audioAlbumName   :: Maybe Text
  , _audioAlbumArt    :: Maybe N.URI
  , _audioArtist      :: Maybe Text
  , _audioCaption     :: Text
  , _audioPlayer      :: HTML
  , _audioPlays       :: Int
  , _audioTitle       :: Maybe Text
  , _audioTrackName   :: Maybe Text
  , _audioTrackNumber :: Maybe Int
  , _audioYear        :: Maybe Int
  } deriving (Show, Eq)

data ChatPostData = ChatPostData
  { _chatCommon   :: PostCommonData
  , _chatDialogue :: [DialogueEntry]
  , _chatTitle    :: Maybe Text
  } deriving (Show, Eq)

data LinkPostData = LinkPostData
  { _linkCommon      :: PostCommonData
  , _linkAuthor      :: Text
  , _linkDescription :: Text
  , _linkExcerpt     :: Text
  , _linkURI         :: N.URI
  , _linkPhotos      :: [Photo]
  , _linkPublisher   :: Text
  , _linkTitle       :: Maybe Text
  } deriving (Show, Eq)

data PhotoPostData = PhotoPostData
  { _photoCommon :: PostCommonData
  , _photoLabel  :: Text
  , _photoData   :: [Photo]
  } deriving (Show, Eq)

data QuotePostData = QuotePostData
  { _quoteCommon :: PostCommonData
  , _quoteSource :: HTML
  , _quoteText   :: Text
  } deriving (Show, Eq)

data TextPostData = TextPostData
  { _textCommon :: PostCommonData
  , _textBody   :: HTML
  , _textTitle  :: Maybe Text
  } deriving (Show, Eq)

data VideoPostData = VideoPostData
  { _videoCommon  :: PostCommonData
  , _videoCaption :: Text
  , _videoPlayers :: [Video]
  } deriving (Show, Eq)

data Post
  = AnswerPost AnswerPostData
  | AudioPost  AudioPostData
  | ChatPost   ChatPostData
  | LinkPost   LinkPostData
  | PhotoPost  PhotoPostData
  | QuotePost  QuotePostData
  | TextPost   TextPostData
  | VideoPost  VideoPostData
  deriving (Show, Eq)

newtype BlogList = BlogList
  { blogList :: [Blog]
  } deriving (Show, Eq)

newtype BlogSummaryList = BlogSummaryList
  { blogSummaryList :: [BlogSummary]
  } deriving (Show, Eq)

newtype PostList = PostList
  { postList :: [Post]
  } deriving (Show, Eq)


--------------------------------------------------------------------------------
-- * Accessors
--
-- Note that most of those are all named with an underscore to be consistent
-- with record fields: the "proper" version is a lens instead.

postType :: Post -> PostType
postType = \case
  AnswerPost {} -> AnswerType
  AudioPost  {} -> AudioType
  ChatPost   {} -> ChatType
  LinkPost   {} -> LinkType
  PhotoPost  {} -> PhotoType
  QuotePost  {} -> QuoteType
  TextPost   {} -> TextType
  VideoPost  {} -> VideoType

_postInfo :: Post -> PostCommonData
_postInfo = \case
  AnswerPost AnswerPostData {..} -> _answerCommon
  AudioPost  AudioPostData  {..} -> _audioCommon
  ChatPost   ChatPostData   {..} -> _chatCommon
  LinkPost   LinkPostData   {..} -> _linkCommon
  PhotoPost  PhotoPostData  {..} -> _photoCommon
  QuotePost  QuotePostData  {..} -> _quoteCommon
  TextPost   TextPostData   {..} -> _textCommon
  VideoPost  VideoPostData  {..} -> _videoCommon

_postId :: Post -> Int
_postId = _commonId . _postInfo

_postBlogName :: Post -> Text
_postBlogName = _commonBlogName . _postInfo

_postBookmarklet :: Post -> Bool
_postBookmarklet = _commonBookmarklet . _postInfo

_postDate :: Post -> UTCTime
_postDate = _commonDate . _postInfo

_postFormat :: Post -> PostFormat
_postFormat = _commonFormat . _postInfo

_postLiked :: Post -> Maybe Bool
_postLiked = _commonLiked . _postInfo

_postMobile :: Post -> Bool
_postMobile = _commonMobile . _postInfo

_postNoteCount :: Post -> Int
_postNoteCount = _commonNoteCount . _postInfo

_postReblogKey :: Post -> Text
_postReblogKey = _commonReblogKey . _postInfo

_postSourceTitle :: Post -> Maybe Text
_postSourceTitle = _commonSourceTitle . _postInfo

_postSourceURI :: Post -> Maybe N.URI
_postSourceURI = _commonSourceURI . _postInfo

_postState :: Post -> PostState
_postState = _commonState . _postInfo

_postTags :: Post -> [Tag]
_postTags = _commonTags . _postInfo

_postURI :: Post -> N.URI
_postURI = _commonURI . _postInfo


--------------------------------------------------------------------------------
-- JSON

instance FromJSON PNGImage where
  parseJSON = withObject "avatar" \o ->
    fmap ImageURI $ parseURI =<< o .: "avatar_url"

instance FromJSON PostFormat where
  parseJSON = withText "post format" \case
    "html"     -> pure HTMLPost
    "markdown" -> pure MarkdownPost
    s          -> fail $ T.unpack s ++ " is not a valid post format"

instance FromJSON PostState where
  parseJSON = withText "post state" \case
    "private"   -> pure PrivatePost
    "draft"     -> pure DraftPost
    "queued"    -> pure QueuedPost
    "published" -> pure PublishedPost
    s           -> fail $ T.unpack s ++ " is not a valid post state"

instance FromJSON PostType where
  parseJSON = withText "post type" \case
    "answer" -> pure AnswerType
    "audio"  -> pure AudioType
    "chat"   -> pure ChatType
    "link"   -> pure LinkType
    "photo"  -> pure PhotoType
    "quote"  -> pure QuoteType
    "text"   -> pure TextType
    "video"  -> pure VideoType
    s        -> fail $ T.unpack s ++ " is not a valid post type"

instance FromJSON Post where
  parseJSON = withObject "post" \o -> do
    common <- parseCommon o
    (o .: "type") >>= \case
      AnswerType -> parseAnswer common o
      AudioType  -> parseAudio  common o
      ChatType   -> parseChat   common o
      LinkType   -> parseLink   common o
      PhotoType  -> parsePhoto  common o
      QuoteType  -> parseQuote  common o
      TextType   -> parseText   common o
      VideoType  -> parseVideo  common o
    where
      parseCommon o = do
        _commonId          <- o .: "id"
        _commonBlogName    <- o .: "blog_name"
        _commonBookmarklet <- o .:? "bookmarklet" .!= False
        _commonDate        <- posixSecondsToUTCTime <$> o .: "timestamp"
        _commonFormat      <- o .: "format"
        _commonLiked       <- o .:? "liked"
        _commonMobile      <- o .:? "mobile" .!= False
        _commonNoteCount   <- o .: "note_count"
        _commonReblogKey   <- o .: "reblog_key"
        _commonSourceTitle <- o .:? "source_title"
        _commonSourceURI   <- traverse parseURI =<< o .:? "source_url"
        _commonState       <- o .: "state"
        _commonTags        <- map Tag <$> o .: "tags"
        _commonURI         <- parseURI =<< o .: "post_url"
        pure $ PostCommonData {..}
      parseAnswer _answerCommon o = do
        _answerText       <- o .: "answer"
        _answerAskingName <- o .: "asking_name"
        _answerAskingURI  <- parseURI =<< o .: "asking_url"
        _answerQuestion   <- o .: "question"
        pure $ AnswerPost AnswerPostData {..}
      parseAudio _audioCommon o = do
        _audioCaption     <- o .: "caption"
        _audioPlayer      <- o .: "player"
        _audioPlays       <- o .: "plays"
        _audioAlbumName   <- o .:? "id3_album"
        _audioAlbumArt    <- traverse parseURI =<< o .:? "id3_album_art"
        _audioArtist      <- o .:? "id3_artist"
        _audioTitle       <- o .:? "id3_title"
        _audioTrackName   <- o .:? "id3_track_name"
        _audioTrackNumber <- o .:? "id3_track_number"
        _audioYear        <- o .:? "id3_year"
        pure $ AudioPost AudioPostData {..}
      parseChat _chatCommon o = do
        _chatDialogue <- o .: "dialogue"
        _chatTitle    <- o .:? "title"
        pure $ ChatPost ChatPostData {..}
      parseLink _linkCommon o = do
        _linkAuthor      <- o .: "author"
        _linkDescription <- o .: "description"
        _linkExcerpt     <- o .: "excerpt"
        _linkURI         <- parseURI =<< o .: "url"
        _linkPhotos      <- o .: "photos"
        _linkPublisher   <- o .: "publisher"
        _linkTitle       <- o .:? "title"
        pure $ LinkPost LinkPostData {..}
      parsePhoto _photoCommon o = do
        _photoLabel <- o .: "caption"
        _photoData  <- o .: "photos"
        pure $ PhotoPost PhotoPostData {..}
      parseQuote _quoteCommon o = do
        _quoteSource <- o .: "source"
        _quoteText   <- o .: "text"
        pure $ QuotePost QuotePostData {..}
      parseText _textCommon o = do
        _textBody  <- o .: "body"
        _textTitle <- o .:? "title"
        pure $ TextPost TextPostData {..}
      parseVideo _videoCommon o = do
        _videoCaption <- o .: "caption"
        _videoPlayers <- o .: "player"
        pure $ VideoPost VideoPostData {..}

instance FromJSON DialogueEntry where
  parseJSON = withObject "dialog entry" \o -> do
    _entryName   <- o .: "name"
    _entryLabel  <- o .: "label"
    _entryPhrase <- o .: "phrase"
    pure $ DialogueEntry {..}

instance FromJSON PhotoSize where
  parseJSON = withObject "photo size" \o -> do
    _photoWidth  <- o .: "width"
    _photoHeight <- o .: "height"
    _photoURI    <- parseURI =<< o .: "url"
    return $ PhotoSize {..}

instance FromJSON Photo where
  parseJSON = withObject "photo" \o -> undefined o

instance FromJSON Video where
  parseJSON = withObject "video" \o -> undefined o

instance FromJSON Blog where
  parseJSON = withObject "blog" \o -> undefined o

instance FromJSON BlogSummary where
  parseJSON = withObject "blog summary" \o -> undefined o

instance FromJSON BlogList where
  parseJSON = withArray "blog list" \a -> undefined a

instance FromJSON BlogSummaryList where
  parseJSON = withArray "blog summary list" \a -> undefined a

instance FromJSON PostList where
  parseJSON = withArray "post list" \a -> undefined a


--------------------------------------------------------------------------------
-- * Internal helpers

parseURI :: Text -> Parser N.URI
parseURI (T.unpack -> s) = N.parseURI s `onNothing` fail (s <> " is not a valid url")


--------------------------------------------------------------------------------
-- * Lenses
--
-- Those are all at the end since TH splices mess up with declaration order.

makePrisms ''Post

makeLenses ''DialogueEntry
makeLenses ''PhotoSize
makeLenses ''Photo
makeLenses ''Video
makeLenses ''Blog
makeLenses ''BlogSummary
makeLenses ''PostCommonData
makeLenses ''AnswerPostData
makeLenses ''AudioPostData
makeLenses ''ChatPostData
makeLenses ''LinkPostData
makeLenses ''PhotoPostData
makeLenses ''QuotePostData
makeLenses ''TextPostData
makeLenses ''VideoPostData

postInfo :: Lens' Post PostCommonData
postInfo f = \case
  AnswerPost d -> AnswerPost <$> answerCommon f d
  AudioPost  d -> AudioPost  <$> audioCommon  f d
  ChatPost   d -> ChatPost   <$> chatCommon   f d
  LinkPost   d -> LinkPost   <$> linkCommon   f d
  PhotoPost  d -> PhotoPost  <$> photoCommon  f d
  QuotePost  d -> QuotePost  <$> quoteCommon  f d
  TextPost   d -> TextPost   <$> textCommon   f d
  VideoPost  d -> VideoPost  <$> videoCommon  f d

postId :: Lens' Post Int
postId = postInfo . commonId

postBlogName :: Lens' Post Text
postBlogName = postInfo . commonBlogName

postBookmarklet :: Lens' Post Bool
postBookmarklet = postInfo . commonBookmarklet

postDate :: Lens' Post UTCTime
postDate = postInfo . commonDate

postFormat :: Lens' Post PostFormat
postFormat = postInfo . commonFormat

postLiked :: Lens' Post (Maybe Bool)
postLiked = postInfo . commonLiked

postMobile :: Lens' Post Bool
postMobile = postInfo . commonMobile

postNoteCount :: Lens' Post Int
postNoteCount = postInfo . commonNoteCount

postReblogKey :: Lens' Post Text
postReblogKey = postInfo . commonReblogKey

postSourceTitle :: Lens' Post (Maybe Text)
postSourceTitle = postInfo . commonSourceTitle

postSourceURI :: Lens' Post (Maybe N.URI)
postSourceURI = postInfo . commonSourceURI

postState :: Lens' Post PostState
postState = postInfo . commonState

postTags :: Lens' Post [Tag]
postTags = postInfo . commonTags

postURI :: Lens' Post N.URI
postURI = postInfo . commonURI



{-
--------------------------------------------------------------------------------
-- * Internal helpers

parseURI :: Text -> Parser N.URI
parseURI s = maybe (fail msg) return $ N.parseURI s
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
  sourceURI   <- ifPresent parseURI =<< o .:? "source_url"
  state       <- o .: "state"
  tags        <- fmap Tag <$> o .: "tags"
  theURI      <- parseURI =<< o .: "post_url"
  return $ PostBase theId theBlogName bookmarklet date format liked mobile noteCount reblogKey sourceTitle sourceURI state tags theURI

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
                            , "tags"           .= fmap getTag (pTags pb)
                            , "type"           .= postType p
                            , "post_url"       .= show (pURI pb)
                            ]
                          , [ "liked"          .= liked
                            | Just liked       <- [pLiked pb]
                            ]
                          , [ "source_title"   .= sourceTitle
                            | Just sourceTitle <- [pSourceTitle pb]
                            ]
                          , [ "source_url"     .= show sourceURI
                            | Just sourceURI   <- [pSourceURI pb]
                            ]
                          ]
  where pb = postBase p



-- instances

instance FromJSON PNGImage where
  parseJSON = withObject "avatar" $ \o -> fmap ImageURI $ parseURI =<< o .: "avatar_url"

instance ToJSON PNGImage where
  toJSON _ = error "[not implemented yet]"


instance FromJSON PostFormat where
  parseJSON = withText "post format" parseFormat
    where parseFormat "html"     = return HTMLPost
          parseFormat "markdown" = return MarkdownPost
          parseFormat s          = fail $ T.unpack s ++ " is not a valid post format"

instance ToJSON PostFormat where
  toJSON = fromText . show

instance FromJSON PostState where
  parseJSON = withText "post state" parseState
    where parseState "private"   = return PrivatePost
          parseState "draft"     = return DraftPost
          parseState "queued"    = return QueuedPost
          parseState "published" = return PublishedPost
          parseState s           = fail $ T.unpack s ++ " is not a valid post state"

instance ToJSON PostState where
  toJSON = fromText . show

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
  toJSON = fromText . show


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


instance FromJSON PhotoSize where
  parseJSON = withObject "photo size" $ \o -> do
    width  <- o .: "width"
    height <- o .: "height"
    url    <- parseURI =<< o .: "url"
    return $ PhotoSize width height url

instance ToJSON PhotoSize where
  toJSON (PhotoSize width height url) =
    object [ "width"  .= width
           , "height" .= height
           , "url"    .= show url
           ]


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


instance FromJSON Video where
  parseJSON = withObject "video" $ \o -> do
    width <- o .: "width"
    code  <- o .: "embed_code"
    return $ Video width code

instance ToJSON Video where
  toJSON (Video width code) = object [ "width"      .= width
                                     , "embed_code" .= code
                                     ]


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
        , "name"                    .= blogName (b :: Blog)
        , "description"             .= blogDescription b
        , "posts"                   .= blogPosts b
        , "updated"                 .= toTimestamp (blogUpdated (b :: Blog))
        , "ask"                     .= blogAsk b
        , "ask_anon"                .= blogAskAnon b
        , "is_blocked_from_primary" .= blogBlocked b
        ] ++ [ "likes"              .= likes
             | Just likes <- [blogLikes b]
             ])
    ]


instance FromJSON BlogSummary where
  parseJSON = withObject "blog summary" $ \o -> do
    name    <- o .: "name"
    uri     <- parseURI =<< o .: "url"
    updated <- fromTimestamp <$> o .: "updated"
    return $ BlogSummary name uri updated

instance ToJSON BlogSummary where
  toJSON b = object
    [ "name"    .= blogName (b :: BlogSummary)
    , "url"     .= show (blogURI b)
    , "updated" .= toTimestamp (blogUpdated (b :: BlogSummary))
    ]



instance ToJSON Post where
  toJSON p = object $ postBaseToObject p ++ details p
    where details (AnswerPost _ answer askingName askingURI question) =
            [ "answer"      .= answer
            , "asking_name" .= askingName
            , "asking_url"  .= show askingURI
            , "question"    .= question
            ]
          details (AudioPost _ mAlbum mAlbumArt mArtist caption player plays mTitle mTrackName mTrackNumber mYear) =
            join [ [ "caption"          .= caption
                   , "player"           .= player
                   , "plays"            .= plays
                   ]
                 , [ "id3_album"        .= album         | Just album        <- [mAlbum]       ]
                 , [ "id3_album_art"    .= show albumArt | Just albumArt     <- [mAlbumArt]    ]
                 , [ "id3_artist"       .= artist        | Just artist       <- [mArtist]      ]
                 , [ "id3_title"        .= title         | Just title        <- [mTitle]       ]
                 , [ "id3_track_name"   .= track_name    | Just track_name   <- [mTrackName]   ]
                 , [ "id3_track_number" .= track_number  | Just track_number <- [mTrackNumber] ]
                 , [ "id3_year"         .= year          | Just year         <- [mYear]        ]
                 ]
          details (ChatPost _ dialogue mTitle) =
            join [ [ "dialogue" .= dialogue                          ]
                 , [ "title"    .= title    | Just title <- [mTitle] ]
                 ]
          details (LinkPost _ author description excerpt link photos publisher mTitle) =
            join [ [ "author"      .= author
                   , "description" .= description
                   , "excerpt"     .= excerpt
                   , "url"         .= show link
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


instance FromJSON BlogList where
  parseJSON = withObject "blog list" $ \o ->
    BlogList <$> o .: "blogs"

instance ToJSON BlogList where
  toJSON (BlogList blogs) = object [ "blogs" .= blogs ]


instance FromJSON BlogSummaryList where
  parseJSON = withObject "blog summary list" $ \o ->
    BlogSummaryList <$> o .: "users"

instance ToJSON BlogSummaryList where
  toJSON (BlogSummaryList blogs) = object [ "users" .= blogs ]


instance FromJSON PostList where
  parseJSON = withObject "post list" $ \o ->
    PostList <$> o .: "posts"

instance ToJSON PostList where
  toJSON (PostList posts) = object [ "posts" .= posts ]



-- decode functions

decodePNG :: N.Response LB.ByteString -> Either Error PNGImage
decodePNG r =
  case lookup N.hContentType $ N.responseHeaders r of
    Just "image/png" -> Right $ ImageRawData $ N.responseBody r
    Just "text/json" -> decodeJSON r
    Just t           -> Left $ ClientError 2 $ "expected image type: " ++ SB.unpack t -- FIXME
    Nothing          -> Left $ ClientError 3 "missing content type header"            -- FIXME

-}
