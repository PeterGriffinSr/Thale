{-# LANGUAGE OverloadedStrings #-}

module Thale.Cli.Subcommand.Verify (runVerify) where

import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.ByteString (readFile)
import Data.Text (unpack)
import Data.Version (showVersion)
import Paths_thale (version)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import System.Info (arch, os)
import Thale.Util.BuildHash (getBuildHash)
import Prelude (IO, Maybe (..), String, pure, putStrLn, ($), (&&), (<>))

extractString :: Maybe Value -> String
extractString (Just (String t)) = unpack t
extractString _ = "N/A"

runVerify :: IO ()
runVerify = do
  homeDir <- getHomeDirectory
  let installPath = homeDir </> ".thale/bin"
      cachePath = homeDir </> ".thale/cache/buildcache.json"

  installExists <- doesDirectoryExist installPath
  cacheExists <- doesFileExist cachePath

  compilerHash <-
    if cacheExists
      then do
        cacheContent <- readFile cachePath
        case decodeStrict cacheContent :: Maybe Value of
          Just (Object obj) -> pure $ extractString $ lookup (fromString "compiler_hash") obj
          _ -> getBuildHash
      else getBuildHash

  compilerVersion <-
    if cacheExists
      then do
        cacheContent <- readFile cachePath
        case decodeStrict cacheContent :: Maybe Value of
          Just (Object obj) -> pure $ extractString $ lookup (fromString "version") obj
          _ -> pure "N/A"
      else pure "N/A"

  putStrLn $ "Version: " <> showVersion version
  putStrLn $ "Build (hash): " <> compilerHash <> " (" <> arch <> ")"
  putStrLn $
    "Install Path: "
      <> if installExists
        then installPath
        else "Install path not found. Please ensure Thale is installed correctly."
  putStrLn $ "Runtime: Native (" <> os <> ")"

  if cacheExists
    then do
      putStrLn "Cache: Enabled"
      putStrLn $ "  Compiler Version: " <> compilerVersion
      putStrLn $ "  Compiler SHA256: " <> compilerHash
    else putStrLn "Cache: Not found"

  if installExists && cacheExists
    then putStrLn "All checks passed."
    else putStrLn "Some checks failed."
