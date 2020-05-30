{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Gateway.Dto
    ( OrderDto
    , IffyOrderPayloadDto,
    ) where

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
import Prelude hiding ( id )

instance ToJSON OrderDto
data OrderDto = OrderDto
    { id           :: UUID
    , status       :: OrderStatusJSON
    , items        :: NonEmpty UUID
    , address      :: Text
    , restaurantId :: UUID
    }
    deriving (Generic)

instance FromJSON IffyOrderPayloadDto
data IffyOrderPayloadDto = IffyOrderPayloadDto
    { items   :: NonEmpty UUID
    , address :: Text
    }
    deriving (Generic)

instance ToDTO OrderDto Order where
    toDTO Order{..} =
        let OrderPayload{..} = orderPayload in
        OrderDto
            { id = unOrderId orderId
            , status = OrderStatusJSON orderStatus
            , items = unOrderOptionId <$> orderPayloadItems
            , address = unAddress orderPayloadAddress
            , restaurantId = unRestaurantId orderRestaurantId
            }

instance FromDTO IffyOrderPayloadDto IffyOrderPayload where
    fromDTO IffyOrderPayloadDto{..} = IffyOrderPayload
        { iffyOrderPayloadItems = IffyOrderOptionId <$> items
        , iffyOrderPayloadAddress = IffyAddress address
        }

newtype OrderStatusJSON = OrderStatusJSON { unOrderStatusJSON :: OrderStatus }
instance ToJSON OrderStatusJSON where
    toJSON = String . toLower . pack . show . unOrderStatusJSON
