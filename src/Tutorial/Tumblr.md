# Tumblr API example

This file aims at showing how to use the Tumblr API, using the Web.Sleep
library. This file is a [literate
Haskell](https://wiki.haskell.org/Literate_programming) file, meaning it is both
rendered as documentation by Github and compilable by GHC. You can load it in
GHCI by running the following commands (you'll need to have the `markdown-unlit`
package installed).

```bash
$ stack install markdown-unlit
$ stack ghci
λ :l src/Tutorial/Tumblr.lhs
```


## Module and extensions

Nothing fancy here.

```haskell
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tutorial.Tumblr where

import           Data.List              as L
import           Control.Exception.Safe
import           Control.Monad.Reader
import           Data.ByteString
import qualified Network.HTTP.Client    as N

-- not needed for normal usage
import           Web.Sleep.Libs.Base
```


## Sleep imports

There are two main entry points to the library:
  * `Web.Sleep.Tumblr` exports everything you'll need,
  * `Web.Sleep.Tumblr.Simple` exports additional helpers.

```haskell
import           Web.Sleep.Tumblr
import           Web.Sleep.Tumblr.Simple
```

This 'Simple' header provides some convenient functions for
prototyping or debugging.


## General design

Each Tumblr API function has a corresponding Haskell function: to get
the posts of a blog, you would use `getBlogPosts`, to post a new text
entry, you would use `postNewBlogText`.

All those functions return a monadic `Query`. You can do two things with those:
  * add optional parameters, using the `&=` operator (see examples below)
  * send that query over the network using one of the three call functions,
     depending on what flavour of error handling you would prefer to use:
    * `call` returns a `m (Either Error Result)`
    * `callT` returns a `MonadThrow m => m Result`
    * `callE` returns a `MonadError Error m => m Result`

(FIXME: error handling is terrible at this point.)

As such, most queries will be of this form:
`callT $ getPostsByType AudioPost &= Limit 20`.

The monad in which those operations happen has several constraints;
but before showing how to implement our own, let's have a look at the
_simple_ version of the API. And even before that, let's talk about
how the network layer is implemented.


## Network

In the common libraries of `Sleep`, you'll find the `NetworkConfig`
struct. This is what the library expects you to provide, in order to
implement the network layer. It holds the network manager, and a
"send" function. It is parameterized by the monad that will be at the
base of your stack (more on this later).

It is of course meant to be used either in `IO`, for actual network
calls, or `Identity`, for tests. Two helper functions,
`defaultIONetworkConfig` and `makeMockNetworkConfig`, provide default
implenentations for those two cases.


## Simple usage

Those _simple_ functions are mostly wrappers around `runReaderT`, providing a
suitable monad stack in which to run our queries. There are two levels of
authentication for Tumblr API calls: either calling with simply an API key, or
by being authentified and signing requests with OAuth. Correspondingly, there
are two main `Simple` functions: `withAPIkey` and `withAuth`, which run in
`IO`. Some monad aliases are also defined:

```haskell

fetchBlogInfo :: N.Manager -> APIKey -> IO Blog
fetchBlogInfo m k = do
  withAPIKey (defaultIONetworkConfig m) k theRequest
  where theRequest :: SimpleAPIKeyMonad IO Blog
        theRequest = callT $ getBlogInfo "datblog.tumblr.com"

fetchBlogFollowers :: N.Manager -> AuthCred -> IO BlogSummaryList
fetchBlogFollowers m a = withAuth (defaultIONetworkConfig m) a theRequest
  where theRequest :: SimpleAuthCredMonad IO BlogSummaryList
        theRequest = callT $ getBlogFollowers "datblog.tumblr.com"
```

The query functions are defined in such a way that trying to call a fuction that
requires authentication will fail to compile in the case where only an APIKey is
provided.

For more information on authentication, please see:
  * Web.Authenticate.OAuth
  * Web.Sleep.Tumblr.Auth
  * Web.Sleep.Tumblr.Simple


## Creating our custom context

For more interesting use cases, let's see how we could use our own
monad to run those queries. The most important requirement is for that
monad to be an instance of `MonadReader`, and for the context held by
the reader to provide what we need. Let's define our context:

```haskell
data Context m = Context { apiKey        :: ByteString
                         , auth          :: AuthCred
                         , networkConfig :: NetworkConfig m
                         , blog          :: String
                         , appData       :: () -- your stuff here
                         }
```

There are several typeclasses of which we need to make our Context an
instance, mostly to retrieve those fields.

To start with, our context has a `NetworkConfig`:

```haskell
instance HasNetworkConfig (Context m) where
  type NetworkConfigBase (Context m) = m
  getNetworkConfig = networkConfig
```

It also provides an API key:

```haskell
instance HasAPIKey (Context m) where
  getAPIKey = APIKey . apiKey
```

For authentication, two typeclasses to implement. Some API functions
require authentication, and will require your type to implement
`HasAuthCred`; the others will use authentication info only if it is
present, and would only require you to implement `MayHaveAuthCred`, if
it didn't already have a default overlappable instance for all types
`a`, which -simply returns `Nothing`. But here, we are authentified,
and we can return an -AuthCred in both cases.



```haskell
-- our context has auth info

instance MayHaveAuthCred (Context m)
instance HasAuthCred (Context m) where
  getAuthCred = auth


-- another context, without auth info
-- uses the global MayHaveAuthCred instance

data NoAuthContext = NoAuthContext
```

Finally, this one is for convenience. All blog query functions have two
variants: one that expects the blog name as an argument, one that gets the blog
from the current context: `getBlogPosts` versus `getPosts`. The `Simple` API
also defines a wrapper around `withReaderT`, `withBlog` that allows one to
locally add a blog name to a given context.

```haskell
instance HasBlogId (Context m) where
  getBlogId = BlogId . blog
```


## Creating our own monad

Nothing more is needed than:

```haskell
type MyTumblrMonad m = ReaderT (Context m) m
```

but you can of course add more transformers to that stack.


## Putting it together

We can directly use our monad with IO:

```haskell
createTextPost :: String -> MyTumblrMonad IO ()
createTextPost content = callT $ postNewText content &= Title "An update!"
```

If we want to abstract the monad at the bottom of the stack, we'll
need to explicitly list all the requirements on `m`:

```haskell
hasDrafts :: ( MonadReader      r m         -- this monad m carries a context r
             , MonadThrow         m         -- this monad m handles exception
             , MonadBase          m         -- this monad m knows what lies at the bottom of the stack (IO or Identity)
             , HasNetworkConfig r           -- this context r holds a network config
             , HasAuthCred      r           -- this context r holds some authentication credentials
             , Base m ~ NetworkConfigBase r -- the network config is in the monad which is the base of the stack
             ) => m Bool                    -- this function returns a boolean value if this monad
hasDrafts = do
  (PostList drafts) <- callT $ getBlogDraftPosts "example.tumblr.com"
  return $ not $ L.null drafts
```

This is quite unwieldy, let's break it down.

`MonadReader`, `MonadBase`, and `HasNetworkConfig` are all required
because we want our context to provide the network configuration. An
alias is defined, `MonadNetwork`, to mean exactly that.

`MonadReader` and `HasAuthCred` are required because the function
`getDraftPosts` requires the context to provide authentication. An
other alias is defined, `MonadAuth`, for just this purpose.

Thanks to those, a generic function such as the one above could be rewritten as:

```haskell
hasDrafts2 :: ( MonadNetwork r m  -- this monad m carries a context r which provides a network config
              , MonadAuth    r m  -- this monad m carries a context r which provides authentication
              , MonadThrow     m  -- this monad m handles exception
              ) => m Bool
hasDrafts2 = do
  (PostList drafts) <- callT $ getBlogDraftPosts "example.tumblr.com"
  return $ not $ L.null drafts
```
