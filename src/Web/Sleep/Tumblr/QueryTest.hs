{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Tumblr.QueryTest (tests) where



-- imports

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.ByteString
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Web.Authenticate.OAuth  as OA

import           Web.Sleep.Common.Config
import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Query



-- helpers

data TestContext     = TestContext     { c1 :: Config Identity, key1 :: ByteString }
data TestAuthContext = TestAuthContext { c2 :: Config Identity, key2 :: ByteString, token :: ByteString }


instance HasConfig TestContext where
  type ConfigBase TestContext = Identity
  getConfig = c1

instance HasConfig TestAuthContext where
  type ConfigBase TestAuthContext = Identity
  getConfig = c2


instance HasAPIKey TestContext where
  getAPIKey = APIKey . key1

instance HasAPIKey TestAuthContext where
  getAPIKey = APIKey . key2

instance MayHaveAuthCred TestAuthContext
instance HasAuthCred     TestAuthContext where
  getAuthCred c = ( tumblrOAuth (key2 c) "SECRET"
                  , OA.Credential [("auth_token", token c)]
                  )


type TestMonad c = ReaderT c Identity

testCtx :: ByteString -> TestContext
testCtx = TestContext $ makeMockConfig []

testAuthCtx :: ByteString -> ByteString -> TestAuthContext
testAuthCtx = TestAuthContext $ makeMockConfig []

u :: MonadConfig c (TestMonad c) => c -> TestMonad c (Query q (TestMonad c)) -> String
u c q = show $ runIdentity $ with c $ toURI =<< q



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
        expect1 = "http://api.tumblr.com:80/v2/blog/test1.tumblr.com/posts?api_key=KEY1"
        actual1 = u (testCtx "KEY1") $ getBlogPosts "test1.tumblr.com"
        expect2 = "http://api.tumblr.com:80/v2/blog/test2.tumblr.com/posts?limit=2&api_key=KEY2"
        actual2 = u (testCtx "KEY2") $ getBlogPosts "test2.tumblr.com" &= Limit 2
        expect3 = "http://api.tumblr.com:80/v2/blog/test3.tumblr.com/posts?auth_token=AUTH3"
        actual3 = u (testAuthCtx "KEY3" "AUTH3") $ getBlogPosts "test3.tumblr.com"
        expect4 = "http://api.tumblr.com:80/v2/blog/test4.tumblr.com/posts?limit=4&auth_token=AUTH4"
        actual4 = u (testAuthCtx "KEY4" "AUTH4") $ getBlogPosts "test4.tumblr.com" &= Limit 4
