module Feature.OrderOption.Persistence.Contract where

import Data.List.NonEmpty
import Feature.OrderOption.Types

class Monad m => Repo m where
    queryAll :: m [OrderOption]
    queryById :: OrderOptionId -> m (Maybe OrderOption)
    filterExisting :: NonEmpty IffyOrderOptionId -> m [OrderOptionId]
    insert :: OrderOption -> m (Either RegisterOptionError ())
    delete :: OrderOptionId -> m (Either DeleteOrderOptionError ())
