module Thale.Util.Source
  ( sourceRef,
    setSource,
    setCurrentFile,
    currentFileRef,
  )
where

import Data.IORef (IORef, newIORef, writeIORef)
import GHC.IO (unsafePerformIO)

{-# NOINLINE sourceRef #-}
sourceRef :: IORef String
sourceRef = unsafePerformIO (newIORef "")

setSource :: String -> IO ()
setSource = writeIORef sourceRef

{-# NOINLINE currentFileRef #-}
currentFileRef :: IORef String
currentFileRef = unsafePerformIO (newIORef "<unknown>")

setCurrentFile :: String -> IO ()
setCurrentFile = writeIORef currentFileRef