{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.ByteString.Lazy.Char8   (pack, unpack)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.String
import           Data.Typeable
import           GHC.Generics
import           Network.HTTP.Client          hiding (withResponse)
import           Network.HTTP.Client.Conduit
import           Network.URL
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Text.Printf
import           Web.Sleep.Tumblr



-- error handling and logging

logInfo, logWarning, logError, logFatal :: String -> IO ()
logInfo    = printf "I: %s\n"
logWarning = printf "W: %s\n"
logError   = printf "E: %s\n"
logFatal   = logError >=> const exitFailure

logFatalOnException :: IO () -> IO ()
logFatalOnException = handle hcf
  where hcf (SomeException e) =
          let typename = show $ typeOf e
              errormsg = show e
          in  logFatal $ printf "uncaught %s: %s" typename errormsg



-- config

data Config = Config { configOutDir :: FilePath
                     , configLastId :: Int
                     } deriving Generic

instance FromJSON Config
instance ToJSON   Config


newtype JSONException = JSONException String deriving (Show, Typeable)

instance Exception JSONException


appName :: String
appName = "TumblrDownloader"

configDir, configFile, lockFile :: String -> IO FilePath
configDir  blog = (</> blog)     <$> getAppUserDataDirectory appName
configFile blog = (</> "config") <$> configDir blog
lockFile   blog = (</> "lock")   <$> configDir blog

loadConfig :: String -> IO Config
loadConfig blog = do
  cf <- configFile blog
  e  <- doesFileExist cf
  if e
  then do
    fc <- readFile cf
    if not $ null fc
    then either (throw . JSONException) return $ eitherDecode $ pack fc
    else defaultConfig
  else defaultConfig
  where defaultConfig = do
          logWarning $ "config not found for " ++ blog
          getHomeDirectory >>= \hd -> return $ Config hd 0


writeConfig :: String -> Config -> IO ()
writeConfig blog config = do
  cf <- configFile blog
  writeFile cf $ unpack $ encode config



-- locking

unlock, lock :: FilePath -> IO ()
unlock = removeFile
lock l = do
  e <- doesFileExist l
  when e $ logFatal $ printf "lock file (%s) already exists: downloader already running?" l
  writeFile l ""

protect :: FilePath -> IO a -> IO a
protect mutex action = finally (lock mutex >> action) $ unlock mutex



-- main

downloadPhoto :: ( MonadResource m
                 , MonadBaseControl IO m
                 , HasHttpManager env
                 , MonadMaybeAuth env m
                 ) =>
                 Config -> (Int, Int, String) -> m ()
downloadPhoto config (pid, index, url) = do
  let filepath = configOutDir config </> show pid ++ "-" ++ show index
  req <- parseRequest url
  liftIO $ logInfo $ printf "downloading %s from %s" filepath url
  withResponse req $ \resp -> responseBody resp $$ sinkFile filepath

downloadPhotos :: ( MonadResource m
                  , MonadBaseControl IO m
                  , HasHttpManager env
                  , MonadMaybeAuth env m
                  , HasNetwork env m
                  , HasBlogId env
                  ) =>
                  Config -> Int -> m Int
downloadPhotos config o = do
  -- liftIO . print . getUri =<< toRequest =<< getPosts &= Offset o &= PType PhotoType &= Limit 20
  (PostList posts) <- callT =<< getPosts &= Offset o &= PType PhotoType &= Limit 20
  let recentEnough = [post | post <- posts, pId (postBase post) > configLastId config]
  when (length recentEnough == 20) $ void $ downloadPhotos config $ o + 20
  liftIO $ logInfo $ printf "%d file(s) remaining" $ length recentEnough
  forM_ [ ( pId $ postBase post
          , index
          , exportURL $ photoURL $ photoOriginal photo
          )
        | post  <- reverse recentEnough
        , (index, photo) <- zip [1..] $ postPhotos post
        ] $ downloadPhoto config
  return $ maximum $ map (pId . postBase) recentEnough

main :: IO ()
main = logFatalOnException $ do
  let blog = "nicuveo.tumblr.com" -- FIXME
  configDir blog >>= createDirectoryIfMissing True
  apiKey <- fromString <$> getEnv "TUMBLR_API_KEY"
  config <- loadConfig blog
  mutex  <- lockFile blog
  lastId <- protect mutex $ runResourceT $ withAPIKey apiKey $ withBlog (BlogId blog) $ downloadPhotos config 0
  writeConfig blog $ config { configLastId = lastId }
