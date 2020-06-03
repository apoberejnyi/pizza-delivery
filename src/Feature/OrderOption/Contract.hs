module Feature.OrderOption.Contract where

import           Data.List.NonEmpty
import           Feature.OrderOption.Error
import           Feature.OrderOption.Types

type GetAllOrderOptions m = m [OrderOption]
type GetOrderOptionById m
    = OrderOptionId -> m (Either GetOrderOptionError OrderOption)
type CheckOrderOptionExistence m
    = NonEmpty IffyOrderOptionId -> m (NonEmpty (Maybe OrderOptionId))
type RegisterOrderOption m
    = OrderOptionPayload -> m (Either RegisterOptionError OrderOptionId)
type DeleteOrderOption m = OrderOptionId -> m (Either DeleteOrderOptionError ())

class Monad m => Service m where
    getAll :: GetAllOrderOptions m
    getById :: GetOrderOptionById m
    checkExistence :: CheckOrderOptionExistence m
    register :: RegisterOrderOption m
    delete :: DeleteOrderOption m
