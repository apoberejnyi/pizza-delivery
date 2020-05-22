module Feature.Order.Contract where

import Feature.Order.Types

class Monad m => Service m where
    placeOrder :: Order -> m ProcessOrderRequest
