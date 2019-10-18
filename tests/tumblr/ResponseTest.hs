{-# OPTIONS_GHC -fno-warn-orphans #-}



-- module

module Web.Sleep.Tumblr.ResponseTest (tests) where



-- imports

import           Control.Arrow
import           Data.Aeson
import           Test.Tasty
import           Test.Tasty.HUnit

import           Web.Sleep.Tumblr.Error
import           Web.Sleep.Tumblr.Response



-- tests

instance FromJSON (Envelope Int) where
  parseJSON = parseEnvelope


tests :: TestTree
tests = testGroup "Web.Sleep.Tumblr.Response" list
  where list = [ testParseRightInt
               , testParseRightUnit
               , testParseLeftWithError
               , testParseLeftWithoutError
               , testMalformedJson
               ]

testParseRightInt :: TestTree
testParseRightInt = testCase "parse right int" assertion
  where assertion = expected @=? getResponse input
        expected  = Right 42 :: Either Error Int
        input     = "\
                    \\n{\
                    \\n  \"meta\": {\
                    \\n    \"status\": 200,\
                    \\n    \"msg\": \"OK\"\
                    \\n  },\
                    \\n  \"response\": 42\
                    \\n}\
                    \\n"

testParseRightUnit :: TestTree
testParseRightUnit = testCase "parse right unit" assertion
  where assertion = expected @=? getResponse input
        expected  = Right () :: Either Error ()
        input     = "\
                    \\n{\
                    \\n  \"meta\": {\
                    \\n    \"status\": 200,\
                    \\n    \"msg\": \"OK\"\
                    \\n  },\
                    \\n  \"response\": \"whatever\"\
                    \\n}\
                    \\n"

testParseLeftWithError :: TestTree
testParseLeftWithError = testCase "parse left with error" assertion
  where assertion = expected @=? getResponse input
        expected  = Left $ ServerError 413 "KO (testFailed)" :: Either Error Int
        input     = "\
                    \\n{\
                    \\n  \"meta\": {\
                    \\n    \"status\": 413,\
                    \\n    \"msg\": \"KO\"\
                    \\n  },\
                    \\n  \"errors\": [\
                    \\n    { \"detail\": \"testFailed\" }\
                    \\n  ]\
                    \\n}\
                    \\n"

testParseLeftWithoutError :: TestTree
testParseLeftWithoutError = testCase "parse left without error" assertion
  where assertion = expected @=? getResponse input
        expected  = Left $ ServerError 418 "teapot" :: Either Error Int
        input     = "\
                    \\n{\
                    \\n  \"meta\": {\
                    \\n    \"status\": 418,\
                    \\n    \"msg\": \"teapot\"\
                    \\n  }\
                    \\n}\
                    \\n"

testMalformedJson :: TestTree
testMalformedJson = testCase "parse malformed json" assertion
  where assertion = expected @=? left clear (getResponse input)
        clear e   = e { errorMsg = "" }
        expected  = Left $ ClientError 1 "" :: Either Error Int
        input     = "\
                    \\n{\
                    \\n  \"meta\": {\
                    \\n    \"status\": 200,\
                    \\n  }\
                    \\n}\
                    \\n"
