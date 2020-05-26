{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Order.Types where

import Base.Types.Address
import Data.List.NonEmpty
import Data.UUID
import Feature.OrderOption.Types
import Feature.Restaurant.Types

type GetAllOrders m = m [Order]
type PlaceOrder m = IffyOrderPayload -> m (Either PlaceOrderError Order)
type GetOrderById m = OrderId -> m (Maybe Order)
type DeleteOrder m = OrderId -> m (Either DeleteOrderError ())

class Monad m => Service m where
    getAll :: GetAllOrders m
    getById :: GetOrderById m
    place :: PlaceOrder m
    delete :: DeleteOrder m

data IffyOrderPayload = IffyOrderPayload
    { orderPayloadItems   :: NonEmpty IffyOrderOptionId
    , orderPayloadAddress :: IffyAddress
    }

data OrderPayload = OrderPayload
    { orderPayloadItems   :: NonEmpty OrderOptionId
    , orderPayloadAddress :: Address
    }

data OrderStatus = Verification
    | Preparing
    | Delivery
    | Completed
    deriving (Show, Read)

newtype OrderId = OrderId { unOrderId :: UUID }
data Order = Order
    { orderId           :: OrderId
    , orderStatus       :: OrderStatus
    , orderPayload      :: OrderPayload
    , orderRestaurantId :: RestaurantId
    }

data PlaceOrderError = NoRestaurantsAvailable
    | AddressNotFound
    | AmbiguousAddress (NonEmpty Address)
    | UnknownOrderOption IffyOrderOptionId

newtype DeleteOrderError = OrderNotFound OrderId
