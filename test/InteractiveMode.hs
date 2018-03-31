{-# LANGUAGE OverloadedStrings #-}

module InteractiveMode where

import           Data.Function
import           Data.String
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import qualified Web.Authenticate.OAuth  as OA

import           Web.Sleep.Tumblr
import           Web.Sleep.Tumblr.Simple



-- debug mode
-- credentials persisted to file

appName, blog :: String
appName = "InteractiveDebugMode"
blog    = "nicuveo.tumblr.com"

configDir, tumblrCreds :: IO FilePath
configDir   = getAppUserDataDirectory appName
tumblrCreds = (</> "tumblr") <$> configDir

getTumblrAuth :: IO AuthCred
getTumblrAuth = do
  m <- defaultManager
  t <- tumblrCreds
  e <- doesFileExist t
  if e
  then do
    [appKey, appSecret, creds] <- lines <$> readFile t
    return $ ( on tumblrOAuth fromString appKey appSecret
             , OA.Credential $ read creds
             )
  else do
    appKey    <- getEnv "TUMBLR_API_KEY"
    appSecret <- getEnv "TUMBLR_API_SECRET"
    let oauth = on tumblrOAuth fromString appKey appSecret
    ac        <- getSimpleAuthCred askOnStdout m oauth
    configDir >>= createDirectoryIfMissing True
    writeFile t $ unlines [appKey, appSecret, show $ OA.unCredential $ snd ac]
    return ac

askOnStdout :: String -> IO String
askOnStdout url = do
  putStrLn $ "Please authorize this tool at: " ++ url
  putStr   $ "Please input the validation token: "
  hFlush stdout
  getLine

debugRun :: SimpleAuthCredBlogMonad IO a -> IO a
debugRun e = do
  ac <- getTumblrAuth
  withAuth ac $ withBlog (BlogId blog) $ e
