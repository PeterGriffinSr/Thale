module Thale.Util.Prompt (prompt) where

import System.IO (hFlush, stdout)
import Prelude (IO, String, getLine, null, pure, putStr, (<>), (>>))

prompt :: String -> String -> IO String
prompt field def = do
  putStr (field <> " [" <> def <> "]: ") >> hFlush stdout
  input <- getLine
  pure (if null input then def else input)