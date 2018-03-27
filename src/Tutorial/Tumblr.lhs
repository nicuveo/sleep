> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}


> -- module

> module Tutorial.Tumblr (createTextPost) where

> import           Control.Exception.Safe
> import           Control.Monad
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

> instance MonadIO m => HasNetwork Context m



> -- main

> createTextPost :: String -> MyTumblrMonad IO ()
> createTextPost content = do
>   (PostList posts) <- callT =<< getPosts &= Offset 10 &= PType PhotoType &= Limit 20
>   when (posts /= []) $ liftIO $ print "test"
