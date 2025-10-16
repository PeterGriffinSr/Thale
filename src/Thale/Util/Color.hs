{-# LANGUAGE CPP #-}

module Thale.Util.Color (enableVirtualTerminal, Color (..), colorize) where

#ifdef mingw32_HOST_OS
import Foreign
import Foreign.C.Types

foreign import ccall "GetStdHandle"e
    c_GetStdHandle :: CInt -> IO CInt

foreign import ccall "GetConsoleMode"
  c_GetConsoleMode :: CInt -> Ptr CInt -> IO CInt

foreign import ccall "SetConsoleMode"
  c_SetConsoleMode :: CInt -> CInt -> IO CInt

stdOutputHandle :: CInt
stdOutputHandle = -11

enableVirtualTerminal :: IO ()
enableVirtualTerminal = do
  handle <- c_GetStdHandle stdOutputHandle
  alloca $ \modePtr -> do
    success <- c_GetConsoleMode handle modePtr
    if success /= 0
      then do
        mode <- peek modePtr
        let enableVT = mode .|. 0x0004
        _ <- c_SetConsoleMode handle enableVT
        pure ()
      else pure ()
#else
enableVirtualTerminal :: IO ()
enableVirtualTerminal = pure ()
#endif

data Color
  = Red
  | Yellow
  | Cyan
  | Green
  | Blue
  | Magenta
  | Reset
  deriving (Show, Eq)

colorCode :: Color -> String
colorCode Red = "\ESC[31m"
colorCode Yellow = "\ESC[33m"
colorCode Cyan = "\ESC[36m"
colorCode Green = "\ESC[32m"
colorCode Blue = "\ESC[34m"
colorCode Magenta = "\ESC[35m"
colorCode Reset = "\ESC[0m"

colorize :: Color -> String -> String
colorize color text = colorCode color ++ text ++ colorCode Reset