module Thale.Util.Source
  ( sourceRef,
    setSource,
  )
where

import Data.IORef (IORef, newIORef, writeIORef)
import GHC.IO (unsafePerformIO)

{-# NOINLINE sourceRef #-}
sourceRef :: IORef String
sourceRef = unsafePerformIO (newIORef "")

setSource :: String -> IO ()
setSource = writeIORef sourceRef