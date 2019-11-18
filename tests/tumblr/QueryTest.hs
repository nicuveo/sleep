{-# LANGUAGE OverloadedStrings #-}



-- module

module QueryTest (tests) where



-- imports

import           Control.Monad.Identity
import qualified Network.HTTP.Client    as N
import           Test.Tasty
import           Test.Tasty.HUnit

import           Web.Sleep.Common.Misc
import           Web.Sleep.Libs.Request
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Query



-- helpers

uk :: APIKeyCommand q => AppKey -> Query q -> String
uk = show . N.getUri ... mkAPIKeyRequest

ua :: OAuthCommand q => AppKey -> Query q -> String
ua = show . N.getUri . runIdentity ... mkOAuthRequest sign
  where sign = return . appendParam ("signed", "true")



-- tests

tests :: TestTree
tests = testGroup "Web.Sleep.Tumblr.Query" list
  where list = [ testGetBlogPosts
               ]

testGetBlogPosts :: TestTree
testGetBlogPosts = testGroup "getBlogPosts" cases
  where cases   = [ testCase "auth:  no, args:  no" $ expect1 @=? actual1
                  , testCase "auth:  no, args: yes" $ expect2 @=? actual2
                  , testCase "auth: yes, args:  no" $ expect3 @=? actual3
                  , testCase "auth: yes, args: yes" $ expect4 @=? actual4
                  ]
        expect1 = "http://api.tumblr.com/v2/blog/test1.tumblr.com/posts?api_key=KEY1"
        actual1 = uk "KEY1" $ getPosts "test1.tumblr.com"
        expect2 = "http://api.tumblr.com/v2/blog/test2.tumblr.com/posts?limit=2&api_key=KEY2"
        actual2 = uk "KEY2" $ getPosts "test2.tumblr.com" &= Limit 2
        expect3 = "http://api.tumblr.com/v2/blog/test3.tumblr.com/posts?api_key=KEY3&signed=true"
        actual3 = ua "KEY3" $ getPosts "test3.tumblr.com"
        expect4 = "http://api.tumblr.com/v2/blog/test4.tumblr.com/posts?limit=4&api_key=KEY4&signed=true"
        actual4 = ua "KEY4" $ getPosts "test4.tumblr.com" &= Limit 4
