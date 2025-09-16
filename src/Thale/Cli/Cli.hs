module Thale.Cli.Cli (runCli) where

import qualified Data.Version as V
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    (<**>),
  )
import Paths_thale (version)
import Thale.Cli.New (runNew)
import Thale.Cli.Options (Command (..))
import Thale.Cli.Parser (commandParser)
import Prelude (IO, putStrLn, (<>), (>>=))

dispatch :: Command -> IO ()
dispatch cmd =
  case cmd of
    Run file -> putStrLn ("Running program: " <> file)
    Build file -> putStrLn ("Building program: " <> file)
    Format file -> putStrLn ("Formatting file: " <> file)
    Check file -> putStrLn ("Typechecking program: " <> file)
    New file -> runNew file
    Repl -> putStrLn "Launching REPL..."
    Version -> putStrLn ("Thale Version " <> V.showVersion version)

runCli :: IO ()
runCli = execParser opts >>= dispatch
  where
    opts =
      info
        (commandParser <**> helper)
        fullDesc