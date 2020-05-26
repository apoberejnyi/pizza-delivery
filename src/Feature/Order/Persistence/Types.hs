module Feature.Order.Persistence.Types where

import Control.Monad.IO.Class
import Feature.Order.Types hiding ( DeleteOrder, delete )

type QueryAllOrders m = m [Order]
type QueryOrderById m = OrderId -> m (Maybe Order)
type InsertOrder m = Order -> m ()
type DeleteOrder m = OrderId -> m (Either DeleteOrderError ())

class MonadIO m => Repo m where
    queryAll :: QueryAllOrders m
    queryById :: QueryOrderById m
    insert :: InsertOrder m
    delete :: DeleteOrder m
