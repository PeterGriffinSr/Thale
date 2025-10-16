module Thale.Cli.Options (Command (..)) where

import Prelude (FilePath, Show)

data Command
  = Run FilePath
  | Build FilePath
  | Format FilePath
  | Check FilePath
  | Repl
  | Version
  | Verify
  | New FilePath
  | NewInteractive
  | Test
  | Doc
  deriving (Show)