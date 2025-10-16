module Thale.Cli.Subcommand.Run (runRun) where

import System.FilePath (replaceExtension)
import Thale.Compiler.Ir (lowerProgram)
import Thale.Compiler.Lexer (alexScanTokens)
import Thale.Compiler.Optimize (optimize)
import Thale.Compiler.Parser (parse)
import Thale.Compiler.Pretty (prettyIR)
import Thale.Util.Source (setSource)
import Prelude (FilePath, IO, readFile, writeFile, ($))

runRun :: FilePath -> IO ()
runRun filePath = do
  source <- readFile filePath
  setSource source
  let ir = optimize $ lowerProgram $ parse $ alexScanTokens source
  writeFile (replaceExtension filePath ".tll") (prettyIR ir)
