{-# LANGUAGE OverloadedStrings #-}

module Thale.Cli.New (runNew, runNewInteractive) where

import Control.Monad (when)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import Prelude

data ProjectConfig = ProjectConfig
  { name :: String,
    version :: String,
    description :: String,
    author :: String,
    license :: String,
    homepage :: String,
    repo :: String
  }

defaults :: ProjectConfig
defaults =
  ProjectConfig
    "my-thale-project"
    "0.1.0"
    "A Thale Project"
    "Your Name <you@example.com>"
    "MIT"
    "https://project.com"
    "https://github.com/project"

prompt :: String -> String -> IO String
prompt field def = do
  putStr (field <> " [" <> def <> "]: ") >> hFlush stdout
  input <- getLine
  pure (if null input then def else input)

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

mainContent :: String
mainContent = "use std.io\n\nval main() {\n    io.println(\"Hello, Thale!\")\n}\n"

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