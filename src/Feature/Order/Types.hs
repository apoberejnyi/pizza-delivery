{-# LANGUAGE DuplicateRecordFields #-}

module Feature.Order.Types where

import           Data.Address
import           Data.Time
import           Data.List.NonEmpty
import           Data.UUID
import           Feature.OrderOption.Types
import           Feature.Restaurant.Types
import           Feature.User.Types

data IffyOrderPayload = IffyOrderPayload
    { items   :: NonEmpty IffyOrderOptionId
    , address :: IffyAddress
    }

data OrderPayload = OrderPayload
    { items   :: NonEmpty OrderOptionId
    , address :: Address
    }

data OrderStatus = Verification
    | Preparing
    | Delivery
    | Completed UTCTime
    deriving (Show, Read)

newtype OrderId = OrderId { unOrderId :: UUID } deriving (Eq)
data Order = Order
    { id :: OrderId
    , status :: OrderStatus
    , payload :: OrderPayload
    , restaurantId :: RestaurantId
    , userId :: UserId
    , placedAt :: UTCTime
    }


