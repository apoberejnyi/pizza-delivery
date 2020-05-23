{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Order.Types where

import Base.Types.Address
import Data.List.NonEmpty
import Data.UUID
import Feature.OrderOption.Types
import Feature.Restaurant.Types

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
