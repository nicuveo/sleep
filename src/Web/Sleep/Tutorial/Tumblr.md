# Tumblr API example

This file aims at showing how to use the Tumblr API, using the
Web.Sleep library. This file is a [literate
Haskell](https://wiki.haskell.org/Literate_programming) file, meaning
it is both rendered as documentation by Github and compilable by
GHC. You can load it in GHCI by running the following commands (you'll
need to have the `markdown-unlit` package installed).

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

module Web.Sleep.Tutorial.Tumblr where

import qualified Network.HTTP.Client    as N
import qualified Network.HTTP.Simple    as S
```


## Sleep imports

There are two main entry points to the library:
  * `Web.Sleep.Tumblr` exports all the base library,
  * `Web.Sleep.Tumblr.Simple` exports additional basic helpers.

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

All those functions return a `Query`. Queries have a type parameter: a
QueryName. This is used to bind some information to queries at the
type level, such as which http method should a query be sent with, or
what data type is expected in the answer.

But most importantly, what you can do with a Query:
  * add optional parameters, using the `&=` operator (see examples below)
  * convert that query to a `Request`; depending on your authentication method, you can use of the following three functions:
    * `mkOAuthRequest` which requires an oauth signing function and an API key,
    * `mkAPIKeyRequest` which only requires an API key,
    * `mkRequest`, which only works for the few queries that do not require any authentication.

As such, sending a request will look something like:
`S.httpLBS $ mkOAuthRequest sign key $ getPostsByType "test.tumblr.com" AudioPost &= Limit 20`.
This library does not provide networking functions, nor does prescribe
a way to organize your program: this example simply uses the `httpLBS`
function from `Network.HTTP.Simple` as an example.

To decode the result, simply use the `decode` function, which takes
the query object you generated, the response's bytestring data, and
returns an `Either Error r`, where `r` is the type associated with
that particular query. Most POST queries use `()`, but you should
still use `decode` on them to see whether the query was successful or
not.

As a result, you could write the following function:

```haskell
getLatestAudioPost :: APIKey -> IO (Either Error Post)
getLatestAudioPost key = do
  let query = getPostsByType "test.tumblr.com" AudioType &= Limit 1
  response <- S.httpLBS $ mkAPIKeyRequest key query
  return $ do
    PostList pl <- decode query response
    return $ head pl
```


## Simple calls

The Simple library provides additional functions and constructs to
help you get started and provide safe defaults to experiment with. At
the heart of them are the `call` series of functions.

The `callK` functions assume you only have an APIKey, and that you are
not authenticated, while the `callA` functions assume you are
authenticated and can sign queries (more on authentication
below). They basically do what the function above does: create the
request, send it over the network, and use decode on the
result. There's three variants for each, depending on how errors are
handled:
  * `callK`  returns an `m (Either Error r)`
  * `callKT` returns an `MonadThrow m => m r`
  * `callKE` returns an `MonadError Error m => m r`.

Those functions assume that the required information (such as APIKey
or signing functions) are stored in a Reader monad. There are two
structures that this library declares that those functions expect:
TumblrK and TumblrA. But if you are not interested in integrating them
in an existing monadic structure, you can simply use the provided
`withApiKey` and `withOAuth` functions, that will provide you with a
simple monadic context:

```haskell
getBlogTitle :: N.Manager -> APIKey -> IO String
getBlogTitle m k =
  withAPIKey (defaultIONetworkConfig m) k $ do
    blog <- callKT $ getBlogInfo "blog1.tumblr.com"
    return $ blogTitle blog

fetchBlogFollowers :: N.Manager -> APIKey -> OAuthFunction IO -> IO BlogSummaryList
fetchBlogFollowers m k s =
  withOAuth (defaultIONetworkConfig m) k s $ callAT $ getBlogFollowers "blog2.tumblr.com"
```


## Simple network

The Simple file also provides some basic network helpers. Within a
`TumblrK` object, network functions are stored in a `NetworkConfig b`,
where `b` is expected to be a "base" monad such as `IO` or
`Identity`. You can use `defaultIONetworkConfig` and
`makeMockNetworkConfig` to generate network config in those two
respective monads.


## Simple authentication

To authenticate yourself against Tumblr, you will need an OAuth
object, that you can create using the `tumblrOAuth` function. With
this, the following steps must be taken:
  * get temporary credentials, by calling `Web.Authenticate.OAuth.getTemporaryCredential`
  * generate an authorization URL, by calling `Web.Authenticate.OAuth.authorizeURL`
  * redirect the user to that URL, where they will get a verifier code
  * use that verifier code to obtain the actual credentials

The `getSimpleAuthCred` does all those steps for you, and simply
expects a callback function that will take the URL and ask the verifer
code in return.
