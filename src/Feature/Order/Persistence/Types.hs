module Feature.Order.Persistence.Types where

import Control.Monad.IO.Class
import Feature.Order.Types

type QueryAllOrders m = m [Order] 
type InsertOrder m = Order -> m ()

class MonadIO m => Repo m where
    queryAll :: QueryAllOrders m
    insert :: InsertOrder m
