{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}



-- module

module Web.Sleep.Tumblr.DataTest (tests) where



-- imports

import           Data.Aeson
import           Data.Maybe
import qualified Data.Time.Clock       as T
import qualified Network.URL           as N
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Data



-- helpers

a :: Arbitrary a => Gen a
a = arbitrary

letter :: Gen Char
letter = elements ['a'..'z']

letters :: Int -> Gen String
letters = _letters $ pure []
  where _letters s 0 = s
        _letters s n = _letters ((:) <$> letter <*> s) $ n-1

instance Arbitrary N.URL where
  arbitrary = do
    secure <- a
    host   <- listOf1 letter
    tld    <- letters 3
    return $ fromJust $ N.importURL $ "http" ++ ['s'|secure] ++ "://" ++ host ++ "." ++ tld

instance Arbitrary T.UTCTime where
  arbitrary = fromTimestamp <$> a

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

instance Arbitrary PostList where
  arbitrary = PostList <$> a `suchThat` (\l -> length l < 5)

checkStability :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
checkStability x = Right x == eitherDecode (encode x)



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
               , testPost
               , testBlogList
               , testPostList
               ]

testPostFormat, testPostState, testPostType            :: TestTree
testDialogueEntry, testPhotoSize, testPhoto, testVideo :: TestTree
testBlog, testPost, testBlogList, testPostList         :: TestTree
testPostFormat    = testProperty "stability of PostFormat"    $ \(x :: PostFormat)    -> checkStability x
testPostState     = testProperty "stability of PostState"     $ \(x :: PostState)     -> checkStability x
testPostType      = testProperty "stability of PostType"      $ \(x :: PostType)      -> checkStability x
testDialogueEntry = testProperty "stability of DialogueEntry" $ \(x :: DialogueEntry) -> checkStability x
testPhotoSize     = testProperty "stability of PhotoSize"     $ \(x :: PhotoSize)     -> checkStability x
testPhoto         = testProperty "stability of Photo"         $ \(x :: Photo)         -> checkStability x
testVideo         = testProperty "stability of Video"         $ \(x :: Video)         -> checkStability x
testBlog          = testProperty "stability of Blog"          $ \(x :: Blog)          -> checkStability x
testPost          = testProperty "stability of Post"          $ \(x :: Post)          -> checkStability x
testBlogList      = testProperty "stability of BlogList"      $ \(x :: BlogList)      -> checkStability x
testPostList      = testProperty "stability of PostList"      $ \(x :: PostList)      -> checkStability x
