module Thale.Util.BuildHash
  ( getBuildHash,
    getGitHash,
    getTimeHash,
  )
where

import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Process (readProcess)
import Prelude
  ( IO,
    Int,
    String,
    divMod,
    floor,
    fromEnum,
    init,
    return,
    reverse,
    take,
    toEnum,
    ($),
    (*),
    (+),
    (-),
    (<),
    (++),
    (<$>)
  )

getBuildHash :: IO String
getBuildHash = getGitHash `catch` handleException
  where
    handleException :: SomeException -> IO String
    handleException _ = ("ts-" ++) <$> getTimeHash

getGitHash :: IO String
getGitHash = do
  result <- readProcess "git" ["rev-parse", "HEAD"] ""
  return (init result)

getTimeHash :: IO String
getTimeHash = do
  t <- getPOSIXTime
  return $ take 16 $ showHex (floor (t * 1000))
  where
    showHex :: Int -> String
    showHex 0 = "0"
    showHex n = reverse (go n)
      where
        go 0 = ""
        go x =
          let (q, r) = x `divMod` 16
              c = if r < 10 then toEnum (fromEnum '0' + r) else toEnum (fromEnum 'a' + r - 10)
           in c : go q
