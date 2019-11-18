{-

A collection of miscellaneous helpers.

-}



-- module

module Web.Sleep.Common.Misc (
  (...),
  with,
  clamp,
  fromTimestamp,
  toTimestamp,
  ) where



-- imports

import           Control.Monad.Reader
import           Data.Time.Clock
import           Data.Time.Clock.POSIX



-- exported functions

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f ... g) x y = f $ g x y
infixr 8 ...

with :: c -> ReaderT c m r -> m r
with = flip runReaderT

clamp :: Ord a => a -> a -> a -> a
clamp a b
  | a < b     = max a . min b
  | otherwise = max b . min a

fromTimestamp :: Integer -> UTCTime
fromTimestamp = posixSecondsToUTCTime . realToFrac . secondsToDiffTime

toTimestamp :: UTCTime -> Integer
toTimestamp = round . utcTimeToPOSIXSeconds
