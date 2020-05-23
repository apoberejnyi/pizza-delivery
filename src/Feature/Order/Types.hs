module Feature.Order.Types where

import Base.Types.Address
import Data.UUID
import Feature.OrderOption.Types
import Feature.Restaurant.Types

data OrderPayload = OrderPayload
    { orderPayloadItems   :: [OrderOptionId]
    , orderPayloadAddress :: Address
    }

data PlaceOrderError = NoRestaurantsAvailable
    | OnAddressResolution ResolveAddressError

newtype OrderId = OrderId { unOrderId :: UUID }
data Order = Order
    { orderId           :: OrderId
    , orderPayload      :: OrderPayload
    , orderRestaurantId :: RestaurantId
    }
