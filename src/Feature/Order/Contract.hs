module Feature.Order.Contract where

import Feature.Order.Types

class Monad m => Service m where
    getAll :: m [Order]
    placeOrder :: OrderPayload -> m (Either PlaceOrderError Order)
