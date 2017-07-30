{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}



-- module

module Web.Sleep.Tumblr () where



-- imports

import           Control.Monad.Reader
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Typeable

import           Web.Sleep.Common.Misc
import           Web.Sleep.Tumblr.Data
import           Web.Sleep.Tumblr.Query
import           Web.Sleep.Tumblr.Methods
import           Web.Sleep.Tumblr.Response












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

type Context = (String, String)

instance HasAPIKey        Context where getAPIKey       = APIKey    . fst
instance HasAuthToken     Context where getAuthToken    = AuthToken . snd
instance MayHaveAuthToken Context where tryGetAuthToken = Just . getAuthToken

test :: IO ()
test = do
  let c = ("key", "token") :: Context
  let q = flip runReader c $ getBlogLikes (BlogId "test") &= Limit 13
  print q
-- case getResponse res of
--   Left  error   -> print $ "failed: " ++ error
--   Right payload -> do
--     print $ blogTitle $ postBlog payload
--     sequence_ $ Prelude.putStrLn . exportURL . photoURL . photoOriginal <$> (postPhotos =<< postList payload)
