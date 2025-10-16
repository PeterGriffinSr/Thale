{-# LANGUAGE OverloadedStrings #-}

module Thale.Cli.Subcommand.New (runNew, runNewInteractive) where

import Control.Monad (when)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Exit (die)
import System.FilePath ((</>))
import Thale.Data.ProjectConfig (ProjectConfig (..), defaults)
import Thale.Data.Toml (tomlContent)
import Thale.Util.Prompt (prompt)
import Prelude (FilePath, IO, String, mapM_, putStrLn, uncurry, writeFile, ($), (++), (<$>), (<*>))

mainContent :: String
mainContent = "use std.io\n\nval main() {\n    io.println(\"Hello, Thale!\")\n}\n"

runNewInteractive :: IO ()
runNewInteractive = do
  putStrLn "Welcome to Thale project setup (interactive). Press Enter to keep the default value."
  cfg <-
    ProjectConfig
      <$> prompt "Project name" (name defaults)
      <*> prompt "Version" (version defaults)
      <*> prompt "Description" (description defaults)
      <*> prompt "Author" (author defaults)
      <*> prompt "License" (license defaults)
      <*> prompt "Homepage" (homepage defaults)
      <*> prompt "Repository" (repo defaults)
  createProject cfg

runNew :: FilePath -> IO ()
runNew projectName = createProject defaults {name = projectName}

createProject :: ProjectConfig -> IO ()
createProject cfg = do
  let projectName = name cfg
  exists <- doesDirectoryExist projectName
  when exists $ die $ "Error: directory \"" ++ projectName ++ "\" already exists."

  mapM_ createDirectory [projectName, projectName </> "src", projectName </> "test"]

  let files =
        [ (projectName </> "src/Main.thl", mainContent),
          (projectName </> "test/Main.thl", mainContent),
          (projectName </> "thale.toml", tomlContent cfg)
        ]

  mapM_ (uncurry writeFile) files
  putStrLn $ "Created new project in " ++ projectName
