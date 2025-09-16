module Thale.Cli.Options (Command (..)) where

import Prelude

data Command
  = Run FilePath
  | Build FilePath
  | Format FilePath
  | Check FilePath
  | New FilePath
  | Repl
  | Version
  deriving (Show)