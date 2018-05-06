{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}



-- module

module Web.Sleep.Tumblr.DataTest (tests) where



-- imports

import           Data.Aeson                   hiding (decode)
import qualified Data.ByteString              as SB
import qualified Data.ByteString.Lazy         as LB
import           Data.Maybe
import           Data.String
import qualified Data.Time.Clock              as T
import qualified Network.HTTP.Client          as N
import qualified Network.HTTP.Client.Internal as N
import qualified Network.HTTP.Types.Status    as N
import qualified Network.HTTP.Types.Version   as N
import qualified Network.URI                  as N
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Network



-- helpers

a :: Arbitrary a => Gen a
a = arbitrary

letter :: Gen Char
letter = elements ['a'..'z']

letters :: Int -> Gen String
letters = _letters $ pure []
  where _letters s 0 = s
        _letters s n = _letters ((:) <$> letter <*> s) $ n-1

instance Arbitrary N.URI where
  arbitrary = do
    secure <- a
    host   <- listOf1 letter
    tld    <- letters 3
    return $ fromJust $ N.parseURI $ "http" ++ ['s'|secure] ++ "://" ++ host ++ "." ++ tld

instance Arbitrary T.UTCTime where
  arbitrary = fromTimestamp <$> a

instance Arbitrary Tag where
  arbitrary = Tag <$> a

instance Arbitrary PostFormat where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PostState where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PostType where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary DialogueEntry where
  arbitrary = DialogueEntry <$> a <*> a <*> a

instance Arbitrary PhotoSize where
  arbitrary = PhotoSize <$> a <*> a <*> a

instance Arbitrary Photo where
  arbitrary = Photo <$> a <*> a <*> a

instance Arbitrary Video where
  arbitrary = Video <$> a <*> a

instance Arbitrary Blog where
  arbitrary = Blog <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a

instance Arbitrary BlogSummary where
  arbitrary = BlogSummary <$> a <*> a <*> a

instance Arbitrary PostBase where
  arbitrary = PostBase <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a

instance Arbitrary Post where
  arbitrary = oneof [ AudioPost  <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a
                    , LinkPost   <$> a <*> a <*> a <*> a <*> a <*> a <*> a <*> a
                    , AnswerPost <$> a <*> a <*> a <*> a <*> a
                    , ChatPost   <$> a <*> a <*> a
                    , PhotoPost  <$> a <*> a <*> a
                    , QuotePost  <$> a <*> a <*> a
                    , TextPost   <$> a <*> a <*> a
                    , VideoPost  <$> a <*> a <*> a
                    ]

instance Arbitrary BlogList where
  arbitrary = BlogList <$> a `suchThat` (\l -> length l < 5)

instance Arbitrary BlogSummaryList where
  arbitrary = BlogSummaryList <$> a `suchThat` (\l -> length l < 5)

instance Arbitrary PostList where
  arbitrary = PostList <$> a `suchThat` (\l -> length l < 5)

checkStability :: (Eq a, ToJSON a, Decode a) => a -> Bool
checkStability x = Right x == decode ( makeMockResponse "text/json" $
                                       makeMockEnvelope $
                                       encode x
                                     )

makeMockEnvelope :: LB.ByteString -> LB.ByteString
makeMockEnvelope x = LB.concat [ "\
                                 \\n{\
                                 \\n  \"meta\": {\
                                 \\n    \"status\": 200,\
                                 \\n    \"msg\": \"OK\"\
                                 \\n  },\
                                 \\n  \"response\": "
                               , x
                               , "\
                                 \\n}\
                                 \\n"
                               ]

makeMockResponse :: SB.ByteString -> LB.ByteString -> N.Response LB.ByteString
makeMockResponse ct body = N.Response { N.responseStatus    = N.mkStatus 200 "OK"
                                      , N.responseHeaders   = [("Content-Type", ct)]
                                      , N.responseBody      = body
                                      , N.responseVersion   = N.http11
                                      , N.responseCookieJar = N.CJ []
                                      , N.responseClose'    = undefined
                                      }



-- tests

tests :: TestTree
tests = testGroup "Web.Sleep.Tumblr.Data" list
  where list = [ testPostFormat
               , testPostState
               , testPostType
               , testDialogueEntry
               , testPhotoSize
               , testPhoto
               , testVideo
               , testBlog
               , testBlogSummary
               , testPost
               , testBlogList
               , testBlogSummaryList
               , testPostList
               , testPNGRawImage
               , testPNGImageURL
               ]

testPostFormat, testPostState, testPostType            :: TestTree
testDialogueEntry, testPhotoSize, testPhoto, testVideo :: TestTree
testBlog, testBlogSummary, testPost                    :: TestTree
testBlogList, testBlogSummaryList, testPostList        :: TestTree
testPostFormat      = testProperty "stability of PostFormat"      $ \(x :: PostFormat)      -> checkStability x
testPostState       = testProperty "stability of PostState"       $ \(x :: PostState)       -> checkStability x
testPostType        = testProperty "stability of PostType"        $ \(x :: PostType)        -> checkStability x
testDialogueEntry   = testProperty "stability of DialogueEntry"   $ \(x :: DialogueEntry)   -> checkStability x
testPhotoSize       = testProperty "stability of PhotoSize"       $ \(x :: PhotoSize)       -> checkStability x
testPhoto           = testProperty "stability of Photo"           $ \(x :: Photo)           -> checkStability x
testVideo           = testProperty "stability of Video"           $ \(x :: Video)           -> checkStability x
testBlog            = testProperty "stability of Blog"            $ \(x :: Blog)            -> checkStability x
testBlogSummary     = testProperty "stability of BlogSummary"     $ \(x :: BlogSummary)     -> checkStability x
testPost            = testProperty "stability of Post"            $ \(x :: Post)            -> checkStability x
testBlogList        = testProperty "stability of BlogList"        $ \(x :: BlogList)        -> checkStability x
testBlogSummaryList = testProperty "stability of BlogSummaryList" $ \(x :: BlogSummaryList) -> checkStability x
testPostList        = testProperty "stability of PostList"        $ \(x :: PostList)        -> checkStability x

testPNGImageURL :: TestTree
testPNGImageURL = testCase "stability of PNGImage (url)" assertion
  where url       = "http://static.tumblr.com/avatars/test.tumblr.com/512"
        image     = ImageURI $ fromJust $ N.parseURI url
        assertion = Right image @=? decode ( makeMockResponse "text/json" $
                                             makeMockEnvelope $
                                             LB.concat [ "{ \"avatar_url\": \""
                                                       , fromString url
                                                       , "\" }"
                                                       ])

testPNGRawImage :: TestTree
testPNGRawImage = testCase "stability of PNGImage (raw)" assertion
  where rawData   = "PNG\NUL\NUL\NULrawdata"
        image     = ImageRawData rawData
        assertion = Right image @=? decode (makeMockResponse "image/png" rawData)
