module Base.Concurrency where

class Concurrent m where
    concurrently :: m a -> m b -> m (a, b)
