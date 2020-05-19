module Feature.OrderOption.Persistence.Contract where

import Feature.OrderOption.Types

class Monad m => Repo m where
    queryAll :: m [OrderOption]
    queryById :: OrderOptionId -> m (Maybe OrderOption)
    insert :: OrderOption -> m (Either RegisterOptionError ())
    delete :: OrderOptionId -> m (Either DeleteOrderOptionError ())
