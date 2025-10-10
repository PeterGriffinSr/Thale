{-# LANGUAGE LambdaCase #-}

module Thale.Cli.Cli (runCli) where

import qualified Data.Version as V
import Options.Applicative (execParser, fullDesc, helper, info, (<**>))
import Paths_thale (version)
import Thale.Cli.New (runNew, runNewInteractive)
import Thale.Cli.Options (Command (..))
import Thale.Cli.Parser (commandParser)
import Thale.Cli.Verify (runVerify)
import Thale.Compiler.Lexer (alexScanTokens)
import Thale.Compiler.Parser (parse)
import Thale.Compiler.Pretty (prettyExpr)

printWith :: String -> FilePath -> IO ()
printWith label file = putStrLn (label <> file)

dispatch :: Command -> IO ()
dispatch = \case
  Run file -> readFile file >>= putStrLn . prettyExpr . parse . alexScanTokens
  Build file -> printWith "Building program: " file
  Check file -> printWith "Typechecking program: " file
  Format _ -> putStrLn "Running Formatter..."
  Repl -> putStrLn "Launching REPL..."
  Version -> putStrLn $ "Thale Version " <> V.showVersion version
  Verify -> runVerify
  New file -> runNew file
  NewInteractive -> runNewInteractive
  Test -> putStrLn "Testing..."
  Doc -> putStrLn "Building docs..."

runCli :: IO ()
runCli = execParser (info (commandParser <**> helper) fullDesc) >>= dispatch