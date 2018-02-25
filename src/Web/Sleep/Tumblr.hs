{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}



-- module

module Web.Sleep.Tumblr (test) where



-- imports

import           Control.Monad.Reader
import qualified Network.HTTP.Client      as N

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

test :: IO ()
test = do
  manager <- N.newManager N.defaultManagerSettings
  with manager $ do
    let apiKey = "FIXME"
    bi <- withKey apiKey $ call =<< getBlogInfo "beesandbombs.tumblr.com"
    case bi of
     Left  err  -> liftIO $ putStrLn $ "failed: " ++ show err
     Right blog -> liftIO $ print blog
