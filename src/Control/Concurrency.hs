module Control.Concurrency where

class Concurrent m where
    concurrently :: m a -> m b -> m (a, b)
    concurrently3 :: m a -> m b -> m c -> m (a, b, c)
