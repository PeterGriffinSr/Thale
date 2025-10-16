module Thale.Data.ProjectConfig (ProjectConfig (..), defaults) where

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
