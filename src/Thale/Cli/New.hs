{-# LANGUAGE OverloadedStrings #-}

module Thale.Cli.New (runNew) where

import Control.Monad (when)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Exit (die)
import System.FilePath ((</>))
import Prelude

runNew :: FilePath -> IO ()
runNew projectName = do
  exists <- doesDirectoryExist projectName
  when exists $
    die $
      "Error: directory \"" ++ projectName ++ "\" already exists."

  createDirectory projectName

  let srcDir = projectName </> "src"
  createDirectory srcDir

  let mainFile = srcDir </> "Main.th"
  writeFile mainFile "use std.io\n\nval main() -> Nil = \n\tio.println(\"Hello, Thale!\")\nin\n"

  let tomlFile = projectName </> "thale.toml"
  writeFile tomlFile $
    "[project]\n"
      ++ "name = \""
      ++ projectName
      ++ "\"\n"
      ++ "version = \"0.1.0\"\n"
      ++ "description = \"A Thale Project\"\n"
      ++ "authors = [\"Your Name <you@example.com>\"]\n"
      ++ "license = \"MIT\"\n"
      ++ "homepage = \"https://project.com\"\n"
      ++ "repository = \"https://github.com/project\"\n\n"
      ++ "[run]\n"
      ++ "main = \"src/Main.th\"\n"
      ++ "run = true\n"

  putStrLn $ "Created new project in " ++ projectName