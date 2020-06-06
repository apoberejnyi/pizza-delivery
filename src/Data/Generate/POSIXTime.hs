module Data.Generate.POSIXTime where

import           Data.Time.Clock.POSIX

class Monad m => POSIXTimeGen m where
    currentTime :: m POSIXTime
