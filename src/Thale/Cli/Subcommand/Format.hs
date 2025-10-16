module Thale.Cli.Subcommand.Format (runFormat) where

runFormat :: FilePath -> IO ()
runFormat filePath = putStrLn $ "Formatting file: " ++ filePath