module Thale.Cli.Parser (commandParser) where

import Options.Applicative
  ( Parser,
    command,
    flag',
    help,
    hsubparser,
    info,
    long,
    metavar,
    progDesc,
    strArgument,
    (<|>),
  )
import Thale.Cli.Options (Command (..))
import Prelude

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "run" (info (Run <$> strArgument (metavar "FILE")) (progDesc "Compile and immediately run a Thale source file"))
        <> command "build" (info (Build <$> strArgument (metavar "FILE")) (progDesc "Compile a Thale source file into an executable artifact"))
        <> command "format" (info (Format <$> strArgument (metavar "FILE")) (progDesc "Format a Thale source file"))
        <> command "check" (info (Check <$> strArgument (metavar "FILE")) (progDesc "Typecheck the input without compiling or running"))
        <> command "repl" (info (pure Repl) (progDesc "Launch an interactive REPL for experimenting with Thale code"))
        <> command "version" (info (pure Version) (progDesc "Show interpeter version"))
        <> command "verify" (info (pure Verify) (progDesc "Show all interpeter infomation"))
        <> command "new" (info (New <$> strArgument (metavar "PROJECT") <|> flag' NewInteractive (long "interactive" <> help "Run interactive project setup")) (progDesc "Create a new Thale project"))
        <> command "test" (info (pure Test) (progDesc "Run test suite"))
        <> command "doc" (info (pure Doc) (progDesc "Build documentation"))
    )
