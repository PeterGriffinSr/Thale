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
  ProjectConfig {name = "my-thale-project", version = "0.1.0", description = "A Thale Project", author = "Your Name <you@example.com>", license = "MIT", homepage = "https://project.com", repo = "https://github.com/project"}
