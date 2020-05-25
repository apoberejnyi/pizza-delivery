{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Order.Types where

import Base.Types.Address
import Data.List.NonEmpty
import Data.UUID
import Feature.OrderOption.Types
import Feature.Restaurant.Types

type GetAllOrders m = m [Order]
type PlaceOrder m = IffyOrderPayload -> m (Either PlaceOrderError Order)

class Monad m => Service m where
    getAll :: GetAllOrders m
    place :: PlaceOrder m

data IffyOrderPayload = IffyOrderPayload
    { orderPayloadItems   :: NonEmpty IffyOrderOptionId
    , orderPayloadAddress :: IffyAddress
    }

data OrderPayload = OrderPayload
    { orderPayloadItems   :: NonEmpty OrderOptionId
    , orderPayloadAddress :: Address
    }

data PlaceOrderError = NoRestaurantsAvailable
    | AddressNotFound
    | AmbiguousAddress (NonEmpty Address)
    | UnknownOrderOption IffyOrderOptionId

newtype OrderId = OrderId { unOrderId :: UUID }
data Order = Order
    { orderId           :: OrderId
    , orderPayload      :: OrderPayload
    , orderRestaurantId :: RestaurantId
    }
