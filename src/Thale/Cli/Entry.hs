{-# LANGUAGE LambdaCase #-}

module Thale.Cli.Entry (runCli) where

import Data.Version (showVersion)
import Options.Applicative (execParser, fullDesc, helper, info, (<**>))
import Paths_thale (version)
import Thale.Cli.Options (Command (..))
import Thale.Cli.Parser (commandParser)
import Thale.Cli.Subcommand.Build (runBuild)
import Thale.Cli.Subcommand.Check (runCheck)
import Thale.Cli.Subcommand.Doc (runDoc)
import Thale.Cli.Subcommand.Format (runFormat)
import Thale.Cli.Subcommand.New (runNew, runNewInteractive)
import Thale.Cli.Subcommand.Repl (runRepl)
import Thale.Cli.Subcommand.Run (runRun)
import Thale.Cli.Subcommand.Test (runTest)
import Thale.Cli.Subcommand.Verify (runVerify)

dispatch :: Command -> IO ()
dispatch = \case
  Run f -> runRun f
  Build f -> runBuild f
  Check f -> runCheck f
  Format f -> runFormat f
  Repl -> runRepl
  Version -> putStrLn $ "Thale Version " <> showVersion version
  Verify -> runVerify
  New f -> runNew f
  NewInteractive -> runNewInteractive
  Test -> runTest
  Doc -> runDoc

runCli :: IO ()
runCli = execParser (info (commandParser <**> helper) fullDesc) >>= dispatch
