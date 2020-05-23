module Feature.OrderOption.Contract where

import Data.List.NonEmpty
import Feature.OrderOption.Types

class Monad m => Service m where
    getAll :: m [OrderOption]
    getById :: OrderOptionId -> m (Maybe OrderOption)
    checkExistence :: NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
    register :: OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)
    delete :: OrderOptionId -> m (Either DeleteOrderOptionError ())

