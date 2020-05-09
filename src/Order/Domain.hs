module Order.Domain where

import           Base.Domain       (Address)
import           Restaurant.Domain (RestaurantId)

data Order = Order
    { orderItems   :: [String]
    , orderAddress :: Address
    }


newtype ProcessOrderRequestId = RequestId String
data ProcessOrderRequest = ProcessOrderRequest
    { requestId           :: ProcessOrderRequestId
    , requestOrder        :: Order
    , requestRestaurantId :: RestaurantId
    }

type PlaceOrder m = Order -> m ProcessOrderRequest
