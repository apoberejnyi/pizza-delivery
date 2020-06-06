module Data.Generate.UTCTime where

import           Data.Time.Clock

class Monad m => UTCTimeGen m where
    currentTime :: m UTCTime
