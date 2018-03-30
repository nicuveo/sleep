> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}


> -- module

> module Tutorial.Tumblr (
>   MyTumblrMonad(..),
>   Context(..),
>   createTextPost,
>   hasDrafts,
>   getLastTenTextPosts,
> ) where

> import           Data.List              as L
> import           Control.Monad.Identity
> import           Control.Exception.Safe
> import           Control.Monad.Reader
> import           Data.ByteString
> import qualified Network.HTTP.Client    as N

> import           Web.Sleep.Tumblr



> newtype MyTumblrMonad m a = MyTumblrMonad {
>   run :: ReaderT Context m a
>   } deriving (Functor,
>               Applicative,
>               Monad,
>               MonadTrans,
>               MonadThrow,
>               MonadReader Context,
>               MonadIO,
>               MonadSign)

> data Context = Context { apiKey  :: ByteString
>                        , auth    :: AuthCred
>                        , manager :: N.Manager
>                        , blog    :: String
>                        }

> instance N.HasHttpManager Context where
>   getHttpManager = manager

> instance MayHaveAuthCred Context
> instance HasAuthCred Context where
>   getAuthCred = auth

> instance HasAPIKey Context where
>   getAPIKey = APIKey . apiKey

> instance HasBlogId Context where
>   getBlogId = BlogId . blog

> instance HasNetwork Context m => HasNetwork Context (MyTumblrMonad m)
> instance HasNetwork Context Identity where send = error "NOT IMPLEMENTED LOL"



> createTextPost :: String -> MyTumblrMonad IO ()
> createTextPost content = callT =<< postNewText content &= Title "An update!"

> hasDrafts :: (HasNetwork Context m, MonadSign m, MonadThrow m) => MyTumblrMonad m Bool
> hasDrafts = do
>   (PostList drafts) <- callT =<< getDraftPosts
>   return $ not $ L.null drafts

> getLastTenTextPosts :: MonadTumblrCall Context m => MyTumblrMonad m (Either Error PostList)
> getLastTenTextPosts = call =<< getPostsByType TextType &= Limit 10
