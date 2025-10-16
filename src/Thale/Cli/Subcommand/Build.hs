module Thale.Cli.Subcommand.Build (runBuild) where

runBuild :: FilePath -> IO ()
runBuild filePath = putStrLn $ "Building program: " ++ filePath