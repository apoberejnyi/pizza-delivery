module Feature.Order.Persistence.Types where

import           Feature.Order.Error
import           Feature.Order.Types

type QueryAllOrders m = m [Order]
type QueryOrderById m = OrderId -> m (Maybe Order)
type InsertOrder m = Order -> m ()
type DeleteOrder m = OrderId -> m (Either DeleteOrderError ())

class Monad m => Repo m where
    queryAll :: QueryAllOrders m
    queryById :: QueryOrderById m
    insert :: InsertOrder m
    delete :: DeleteOrder m
