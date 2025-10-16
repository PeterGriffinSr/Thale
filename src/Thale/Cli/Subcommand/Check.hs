module Thale.Cli.Subcommand.Check (runCheck) where

runCheck :: FilePath -> IO ()
runCheck filePath = putStrLn $ "Typechecking program: " ++ filePath