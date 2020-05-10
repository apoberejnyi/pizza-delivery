{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Order.Domain where

import Base.Domain (Address)
import Data.Aeson
import Data.Text
import Data.UUID
import GHC.Generics
import Restaurant.Domain (RestaurantId)

type PlaceOrder m = Order -> m ProcessOrderRequest
type GetRequests m = RestaurantId -> m [ProcessOrderRequest]
type StartDelivery m = ProcessOrderRequest -> m ()
type FulfilRequest m = ProcessOrderRequest -> m ()

data Order = Order
    { orderItems   :: [Text]
    , orderAddress :: Address
    }

instance FromJSON Order where
   parseJSON (Object v) =
       Order <$> v .: "items"
             <*> v .: "address"

instance ToJSON Order where
    toJSON (Order items address) = object
        [ "items" .= items
        , "address" .= address
        ]

newtype ProcessOrderRequestId = RequestId UUID deriving (Eq, Generic)
instance ToJSON ProcessOrderRequestId

data ProcessOrderRequest = ProcessOrderRequest
    { requestId           :: ProcessOrderRequestId
    , requestOrder        :: Order
    , requestRestaurantId :: RestaurantId
    }

instance ToJSON ProcessOrderRequest where
    toJSON (ProcessOrderRequest id order restaurantId) = object
        [ "id" .= id
        , "order" .= order
        , "restaurantId" .= restaurantId
        ]

