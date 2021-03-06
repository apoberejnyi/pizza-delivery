{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Order.Gateway.DTO
  ( OrderDto
  , IffyOrderPayloadDto
  )
where

import           Data.Address
import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text
import           Data.UUID
import           Feature.Order.Types
import           Feature.OrderOption.Types
import           Feature.Restaurant.Types
import           Gateway.Util
import           GHC.Generics
import           Prelude                 hiding ( id )

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
  toDTO Order {..} =
    let OrderPayload {..} = payload
    in  OrderDto { id           = unOrderId id
                 , status       = OrderStatusJSON status
                 , items        = unOrderOptionId <$> items
                 , address      = unAddress address
                 , restaurantId = unRestaurantId restaurantId
                 }

instance FromDTO IffyOrderPayloadDto IffyOrderPayload where
  fromDTO IffyOrderPayloadDto {..} = IffyOrderPayload
    { items   = IffyOrderOptionId <$> items
    , address = IffyAddress address
    }

newtype OrderStatusJSON = OrderStatusJSON { unOrderStatusJSON :: OrderStatus }
instance ToJSON OrderStatusJSON where
  toJSON = String . toLower . pack . show . unOrderStatusJSON
