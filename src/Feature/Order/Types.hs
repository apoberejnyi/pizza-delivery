module Feature.Order.Types where

import Base.Types.Address (Address)
import Data.Text (Text)
import Data.UUID (UUID)
import Feature.Restaurant.Types (RestaurantId)

data Order = Order
    { orderItems   :: [Text]
    , orderAddress :: Address
    }

data PlaceOrderError = NoRestaurantsAvailable

newtype ProcessOrderRequestId = ProcessOrderRequestId UUID
data ProcessOrderRequest = ProcessOrderRequest
    { requestId           :: ProcessOrderRequestId
    , requestOrder        :: Order
    , requestRestaurantId :: RestaurantId
    }
