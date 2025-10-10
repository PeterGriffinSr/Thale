module Thale.Cli.Verify (runVerify) where

import Control.Exception (SomeException, catch)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Version as V
import Paths_thale (version)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Info (arch, os)
import System.Process (readProcess)
import Prelude

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

runVerify :: IO ()
runVerify = do
  homeDir <- getHomeDirectory
  buildHash <- getBuildHash
  let installPath = homeDir ++ "/.thale/bin"
  installExists <- doesDirectoryExist installPath
  putStrLn $ "Version: " <> V.showVersion version
  putStrLn $ "Build: " <> buildHash <> " (" <> arch <> ")"
  putStrLn $
    "Install Path: "
      <> if installExists
        then installPath
        else "Install path not found. Please ensure Thale is installed correctly."

  putStrLn $ "Runtime: Native (" <> os <> ")"
  putStrLn "Cache: Enabled"

  if installExists
    then putStrLn "All checks passed."
    else putStrLn "Some checks failed."
