{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Feature.Order.Gateway.Dto where

import Base.HTTP
import Base.Types.Address
import Data.Aeson
import Data.List.NonEmpty
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
    , items        :: NonEmpty UUID
    , address      :: Text
    , restaurantId :: UUID
    }
    deriving (Show, Generic)

instance FromJSON IffyOrderPayloadDto
instance ToJSON IffyOrderPayloadDto
data IffyOrderPayloadDto = IffyOrderPayloadDto
    { items   :: NonEmpty UUID
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

instance FromDTO IffyOrderPayloadDto IffyOrderPayload where
    fromDTO IffyOrderPayloadDto{..} = IffyOrderPayload
        { orderPayloadItems = IffyOrderOptionId <$> items
        , orderPayloadAddress = IffyAddress address
        }

