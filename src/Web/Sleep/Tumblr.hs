{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Tumblr (test) where



-- imports

import           Control.Monad.Reader
import           Data.ByteString.Char8    (pack)

import           Web.Sleep.Tumblr.Auth
import           Web.Sleep.Tumblr.Context
import           Web.Sleep.Tumblr.Query


-- blog following

-- data QFollowing;
-- instance QueryParam QFollowing AuthToken;
-- instance QueryParam QFollowing Limit;
--
-- getBlogFollowing :: (MonadReader c m, HasAuthToken c) => BlogId -> m (Query QFollowing PostList)
-- getBlogFollowing (BlogId bid) = do
--   token <- asks getAuthToken
--   return $ Query (apiRoot ++ "blog/" ++ bid ++ "/following") M.empty &= token
--
-- getFollowing :: (MonadReader c m, HasAuthToken c, HasBlogId c) => m (Query QFollowing PostList)
-- getFollowing = asks getBlogId >>= getBlogFollowing



-- debug

input :: String -> IO String
input prompt = do
  putStr prompt
  getLine

validate :: String -> IO String
validate url = do
  putStrLn $ "Please authorize: " ++ url
  input "Validation token: "

test :: IO ()
test = do
  appKey    <- pack <$> input "API key:          "
  appSecret <- pack <$> input "API secret:       "
  let oauth = tumblrOAuth appKey appSecret
  authCred  <- getSimpleDebugAuthCred validate oauth
  withAuth authCred $ do
    bi <- call =<< getBlogInfo "beesandbombs.tumblr.com"
    case bi of
      Left  err  -> liftIO $ putStrLn $ "failed: " ++ show err
      Right blog -> liftIO $ print blog
    bpd <- callT =<< getBlogPostsDraft "nicuveo.tumblr.com"
    liftIO $ print bpd
