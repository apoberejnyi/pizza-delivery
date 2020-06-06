module Feature.Order.Contract where

import           Feature.Order.Error
import           Feature.Order.Types
import           Feature.User.Types

type GetAllOrders m = m [Order]
type PlaceOrder m
  = UserId -> IffyOrderPayload -> m (Either PlaceOrderError Order)
type GetOrderById m = OrderId -> m (Either GetOrderError Order)
type DeleteOrder m = OrderId -> m (Either DeleteOrderError ())

class Monad m => Service m where
    getAll :: GetAllOrders m
    getById :: GetOrderById m
    place :: PlaceOrder m
    delete :: DeleteOrder m
