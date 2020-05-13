module Feature.OrderOption.Contract where

import Feature.OrderOption.Types

class Monad m => Service m where
    getAll :: m [OrderOption]
    register :: OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)

class Monad m => Repo m where
    queryAll :: m [OrderOption]
    insert :: OrderOption -> m (Either RegisterOptionError ())
