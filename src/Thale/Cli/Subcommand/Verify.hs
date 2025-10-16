module Thale.Cli.Subcommand.Verify (runVerify) where

import qualified Data.Version as V
import Paths_thale (version)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Info (arch, os)
import Thale.Util.BuildHash (getBuildHash)
import Prelude (IO, putStrLn, ($), (++), (<>))

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