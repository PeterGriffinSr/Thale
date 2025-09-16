module Thale.Cli.Parser (commandParser) where

import Options.Applicative
  ( Parser,
    command,
    hsubparser,
    info,
    metavar,
    progDesc,
    strArgument,
  )
import Thale.Cli.Options (Command (..))
import Prelude

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "run"
        ( info
            (Run <$> strArgument (metavar "FILE"))
            (progDesc "Compile and immediately run a Thale source file")
        )
        <> command
          "build"
          ( info
              (Build <$> strArgument (metavar "FILE"))
              (progDesc "Compile a Thale source file into an executable artifact")
          )
        <> command
          "format"
          ( info
              (Format <$> strArgument (metavar "FILE"))
              (progDesc "Format a Thale source file")
          )
        <> command
          "check"
          ( info
              (Check <$> strArgument (metavar "FILE"))
              (progDesc "Typecheck the input without compiling or running")
          )
        <> command
          "new"
          ( info
              (New <$> strArgument (metavar "PROJECT"))
              (progDesc "Create a new Thale project in a directory named PROJECT")
          )
        <> command
          "repl"
          ( info
              (pure Repl)
              (progDesc "Launch an interactive REPL for experimenting with Thale code")
          )
        <> command
          "version"
          ( info
              (pure Version)
              (progDesc "Show compiler version")
          )
    )
