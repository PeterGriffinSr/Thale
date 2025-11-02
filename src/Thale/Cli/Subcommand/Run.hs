{-# LANGUAGE ScopedTypeVariables #-}

module Thale.Cli.Subcommand.Run (runRun) where

import Control.Exception (evaluate, try)
import Control.Exception.Base (SomeException)
import System.FilePath (replaceExtension)
import Thale.Compiler.Ir (lowerProgram)
import Thale.Compiler.Lexer (alexScanTokens)
import Thale.Compiler.Optimize (optimize)
import Thale.Compiler.Parser (parse)
import Thale.Compiler.Pretty (prettyIR)
import Thale.Util.Source (setCurrentFile, setSource)
import Prelude (Applicative (pure), Either (..), FilePath, IO, readFile, writeFile, ($))

runRun :: FilePath -> IO ()
runRun filePath = do
  source <- readFile filePath
  setSource source
  setCurrentFile filePath

  result <-
    try $
      evaluate $
        optimize $
          lowerProgram $
            parse $
              alexScanTokens source

  case result of
    Left (_ :: SomeException) -> pure ()
    Right ir -> do
      irTextResult <- try $ evaluate (prettyIR ir)
      case irTextResult of
        Left (_ :: SomeException) -> pure ()
        Right irText -> do
          writeFile (replaceExtension filePath ".tll") irText