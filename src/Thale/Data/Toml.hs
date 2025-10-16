module Thale.Data.Toml (tomlContent) where

import Thale.Data.ProjectConfig (ProjectConfig (..))
import Prelude (String, show, unlines, (<>))

tomlContent :: ProjectConfig -> String
tomlContent cfg =
  unlines
    [ "[project]",
      "name = " <> show (name cfg),
      "version = " <> show (version cfg),
      "description = " <> show (description cfg),
      "authors = [" <> show (author cfg) <> "]",
      "license = " <> show (license cfg),
      "homepage = " <> show (homepage cfg),
      "repository = " <> show (repo cfg),
      "",
      "[run]",
      "main = \"src/Main.thl\"",
      "run = true"
    ]
