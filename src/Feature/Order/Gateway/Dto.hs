{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Feature.Order.Gateway.Dto where

import Base.HTTP
import Base.Types.Address
import Data.Aeson
import Data.Text
import Data.UUID
import Feature.Order.Types
import Feature.OrderOption.Types
import Feature.Restaurant.Types
import GHC.Generics
import Prelude hiding (id)

instance FromJSON OrderDto
instance ToJSON OrderDto
data OrderDto = OrderDto
    { id           :: UUID
    , items        :: [UUID]
    , address      :: Text
    , restaurantId :: UUID
    }
    deriving (Show, Generic)

instance FromJSON OrderPayloadDto
instance ToJSON OrderPayloadDto
data OrderPayloadDto = OrderPayloadDto
    { items   :: [UUID]
    , address :: Text
    }
    deriving (Show, Generic)

instance ToDTO OrderDto Order where
    toDTO Order{..} =
        let OrderPayload{..} = orderPayload in
        OrderDto
            { id = unOrderId orderId
            , items = unOrderOptionId <$> orderPayloadItems
            , address = unAddress orderPayloadAddress
            , restaurantId = unRestaurantId orderRestaurantId
            }

instance FromDTO OrderPayloadDto OrderPayload where
    fromDTO OrderPayloadDto{..} = OrderPayload
        { orderPayloadItems = OrderOptionId <$> items
        , orderPayloadAddress = Address address
        }

